module Parser 
( convertStackOutput
)
where

data ParseState = ParseState {
  _currentParser :: Parser
}

data Parser = 
    WaitingForError
  | GatheringErrorMessage [String]

convertStackOutput :: String -> String
convertStackOutput allInput = undefined $ foldr processLine (ParseState WaitingForError) $ lines allInput
  where
  processLine :: String -> ParseState -> ParseState
  processLine line = parseLine . _currentParser
    where
    lineContent :: [String]
    lineContent = words line
    parseLine :: Parser -> ParseState
    parseLine WaitingForError =
      if any (`elem` ["error:", "warning:"]) lineContent
      then ParseState $ GatheringErrorMessage lineContent
      else ParseState WaitingForError
