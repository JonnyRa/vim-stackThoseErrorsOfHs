{-# Language RecordWildCards #-}
{-# Language OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Parser 
( convertStackOutput
)
where

import Prelude hiding (unlines, unwords, words, lines)
import Data.List.Extra ((!?))
import Data.Tuple.Extra
import Data.Text (Text, unlines, unwords, words, lines)
import qualified Data.Text as Text
import Data.Maybe
import Data.Foldable
import Data.DList (DList)
import qualified Data.DList as DList
import Text.Regex.TDFA
import Text.RawString.QQ

data ParseState = ParseState {
  _currentParser :: Parser
, _errors :: DList ErrorInformation
}

data Parser = 
    WaitingForError
  | GatheringErrorMessage [Text]
  deriving Show

data ErrorInformation = ErrorInformation {
  _errorLocation :: Text
, _errorType :: Text
, _errorMessage :: Text
} deriving Show

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
      if any (`elem` ["error:", "warning:"]) lineContent && (firstLetter =<< listToMaybe lineContent) == Just '/'
      then changeToParser (GatheringErrorMessage lineContent) currentState
      else changeToParser WaitingForError currentState
      where
      firstLetter :: Text -> Maybe Char
      firstLetter = fmap fst . Text.uncons

    parseLine (GatheringErrorMessage errorLine) =
      ParseState WaitingForError $ _errors currentState `DList.snoc` makeInformation errorLine lineContent

changeToParser :: Parser -> ParseState -> ParseState
changeToParser parser state = state {_currentParser = parser}

makeInformation :: [Text] -> [Text] -> ErrorInformation
makeInformation errorLine firstErrorMessageLine =
  ErrorInformation {
    _errorLocation = head adjustedError
  , _errorType = unwords $ drop 1 adjustedError
  , _errorMessage = unwords adjustedMessage
  }

  where
  adjustedError, adjustedMessage :: [Text]
  (adjustedError, adjustedMessage) = 
    adjustment (errorLine, firstErrorMessageLine)
    where
    adjustment :: ([Text], [Text]) -> ([Text], [Text])
    adjustment = 
      if errorLine !? 1 == Just ">"
      then both (drop 2) 
      else id
        
outputForVim :: ErrorInformation -> Text
outputForVim ErrorInformation{..} = 
  Text.intercalate "" [_errorLocation, _errorType, _errorMessage]
