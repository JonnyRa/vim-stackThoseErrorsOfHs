{-# Language RecordWildCards #-}

module Parser 
( convertStackOutput
)
where

import Data.List
import Data.List.Extra ((!?))
import Data.Tuple.Extra

data ParseState = ParseState {
  _currentParser :: Parser
, _errorsInReverseOrder :: [ErrorInformation]
}

data Parser = 
    WaitingForError
  | GatheringErrorMessage [String]

data ErrorInformation = ErrorInformation {
  _errorLocation :: String
, _errorType :: String
, _errorMessage :: String
}

convertStackOutput :: String -> String
convertStackOutput allInput = convertToOutput $ _errorsInReverseOrder $ foldr processLine (ParseState WaitingForError []) $ lines allInput
  where
  convertToOutput :: [ErrorInformation] -> String
  convertToOutput = unlines . map outputForVim

  processLine :: String -> ParseState -> ParseState
  processLine line currentState = parseLine $ _currentParser currentState
    where
    lineContent :: [String]
    lineContent = words line
    parseLine :: Parser -> ParseState
    parseLine WaitingForError =
      if any (`elem` ["error:", "warning:"]) lineContent
      then changeToParser (GatheringErrorMessage lineContent) currentState
      else changeToParser WaitingForError currentState

    parseLine (GatheringErrorMessage errorLine) =
      ParseState WaitingForError $ makeInformation errorLine lineContent: _errorsInReverseOrder currentState

changeToParser :: Parser -> ParseState -> ParseState
changeToParser parser state = state {_currentParser = parser}

makeInformation :: [String] -> [String] -> ErrorInformation
makeInformation errorLine firstErrorMessageLine =
  ErrorInformation {
    _errorLocation = head adjustedError
  , _errorType = unwords $ drop 1 adjustedError
  , _errorMessage = unwords adjustedMessage
  }

  where
  adjustedError, adjustedMessage :: [String]
  (adjustedError, adjustedMessage) = 
    adjustment (errorLine, firstErrorMessageLine)
    where
    adjustment :: ([String], [String]) -> ([String], [String])
    adjustment = 
      if errorLine !? 1 == Just ">"
      then both (drop 2) 
      else id
        
outputForVim :: ErrorInformation -> String
outputForVim ErrorInformation{..} = 
  intercalate ":" [_errorLocation, _errorType, _errorMessage]
