{-# Language RecordWildCards #-}
{-# Language OverloadedStrings #-}
{-# Language TemplateHaskell #-}
{-# Language RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}

module Parser 
( convertStackOutput
)
where

import Prelude hiding (unlines, unwords, words, lines)
import Data.List.Extra ((!?))
import Data.Text (Text, unlines, unwords, words, lines)
import qualified Data.Text as Text
import Data.Maybe
import Data.Foldable
import Data.DList (DList)
import qualified Data.DList as DList
import Text.Regex.TDFA
import Lens.Micro hiding (both)
import Lens.Micro.TH
import Data.Bifunctor
import Text.RawString.QQ

data ParseState = ParseState {
  _currentParser :: Parser
, _errors :: DList ErrorInformation
}

data Parser = 
    WaitingForError
  | GatheringErrorMessage GatherState
  deriving Show

data GatherState = GatherState {
  _errorLine :: [Text] 
, _detailedMessage :: DList [Text]
} deriving Show

data ErrorInformation = ErrorInformation {
  _errorLocation :: Text
, _errorType :: Text
, _errorMessage :: Text
} deriving Show

makeLenses ''ParseState
makeLenses ''GatherState

convertStackOutput :: Text -> Text
convertStackOutput allInput = convertToOutput $ toList $ _errors $ foldl' (flip processLine) (ParseState WaitingForError DList.empty) $ lines allInput
  where
  convertToOutput :: [ErrorInformation] -> Text
  convertToOutput = unlines . map outputForVim

  processLine :: Text -> ParseState -> ParseState
  processLine line currentState = parseLine $ _currentParser currentState
    where
    lineContent :: [Text]
    lineContent = 
      words $ if Text.length preMatch > 0 
        then preMatch
        else postMatch
      where
      --regex to look for
      --`package-name       > ` prefixed on the start of lines.  Might not have any spaces between the package name and >
      --if this matches then the first match is the second item with pre/post match before/after
      --if it doesn't then everything ends up in the first bit of the tuple
      preMatch, postMatch :: Text
      (preMatch,_,postMatch) = line =~ regex :: (Text, Text, Text)
        where
        regex :: Text
        regex = "^[-[:word:]]+ *>"
      
    parseLine :: Parser -> ParseState
    parseLine WaitingForError =
      if any (`elem` ["error:", "warning:"]) lineContent && firstLetterOfLine == Just '/'
      then changeToParser (GatheringErrorMessage $ GatherState lineContent mempty) currentState
      else changeToParser WaitingForError currentState

    parseLine (GatheringErrorMessage gatherState)
      -- relying on error messages being terminated with blocks that look like this:
      --    |
      --470 |   getLightDiagram crewDiagId scheme <|> (getDiagramForValidationFromShadow =<< getShadowDiagram crewDiagId scheme)
      --    |                                     ^^^
      | firstLetterOfLine == Just '|' = changeToParser WaitingForError $ addError (makeInformation gatherState) currentState 
      --continue gathering message but just ignore this line
      | isUnrelatedInterleavedLine = currentState
      | otherwise = changeToParser (GatheringErrorMessage $ addErrorLine lineContent gatherState) currentState 
      
      where
      --this is to match/ignore things like:
      --[807 of 873] Compiling Handler.Rosters.Gantt [Foundation.App changed]
      --which sometimes get interleaved
      isUnrelatedInterleavedLine :: Bool
      isUnrelatedInterleavedLine =
        line =~ regex
        where
        regex :: Text
        regex = [r|\[[[:digit:]]+ of [[:digit:]]+\]|]

    firstLetterOfLine :: Maybe Char
    firstLetterOfLine = firstLetterOf =<< listToMaybe lineContent
      where
      firstLetterOf :: Text -> Maybe Char
      firstLetterOf = fmap fst . Text.uncons

addErrorLine :: [Text] -> GatherState -> GatherState 
addErrorLine = addToDList detailedMessage

changeToParser :: Parser -> ParseState -> ParseState
changeToParser = set currentParser

addError :: ErrorInformation -> ParseState -> ParseState
addError = addToDList errors

addToDList :: Lens' container (DList a) -> a -> container -> container
addToDList setter newThing = over setter (`DList.snoc` newThing)

makeInformation :: GatherState -> ErrorInformation
makeInformation GatherState{..} =
  ErrorInformation {
    _errorLocation = head adjustedError
  , _errorType = unwords $ drop 1 adjustedError
  , _errorMessage = unwords $ map unwords $ toList adjustedMessage
  }

  where
  adjustedError :: [Text]
  adjustedMessage :: DList [Text]
  (adjustedError, adjustedMessage) = 
    adjustment (_errorLine, _detailedMessage)
    where
    adjustment :: ([Text], DList [Text]) -> ([Text], DList [Text])
    adjustment = 
      bimap editOne (fmap editOne)
      where
      editOne :: [Text] -> [Text]
      editOne = 
        if _errorLine !? 1 == Just ">"
        then (drop 2) 
        else id
        
outputForVim :: ErrorInformation -> Text
outputForVim ErrorInformation{..} = 
  Text.intercalate "" [_errorLocation, _errorType, _errorMessage]
