{-# Language RecordWildCards #-}
{-# Language OverloadedStrings #-}

module Parser 
( convertStackOutput
)
where

import Prelude hiding (unlines, unwords, words, lines)
import Data.List.Extra ((!?))
import Data.Tuple.Extra
import Data.Text (Text, unlines, unwords, words, lines)
import qualified Data.Text as Text

data ParseState = ParseState {
  _currentParser :: Parser
, _errors :: [ErrorInformation]
}

data Parser = 
    WaitingForError
  | GatheringErrorMessage [Text]

data ErrorInformation = ErrorInformation {
  _errorLocation :: Text
, _errorType :: Text
, _errorMessage :: Text
}

convertStackOutput :: Text -> Text
convertStackOutput allInput = convertToOutput $ _errors $ foldr processLine (ParseState WaitingForError []) $ lines allInput
  where
  convertToOutput :: [ErrorInformation] -> Text
  convertToOutput = unlines . map outputForVim

  processLine :: Text -> ParseState -> ParseState
  processLine line currentState = parseLine $ _currentParser currentState
    where
    lineContent :: [Text]
    lineContent = words line
    parseLine :: Parser -> ParseState
    parseLine WaitingForError =
      if any (`elem` ["error:", "warning:"]) lineContent
      then changeToParser (GatheringErrorMessage lineContent) currentState
      else changeToParser WaitingForError currentState

    parseLine (GatheringErrorMessage errorLine) =
      ParseState WaitingForError $ makeInformation errorLine lineContent: _errors currentState

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
  Text.intercalate ":" [_errorLocation, _errorType, _errorMessage]
