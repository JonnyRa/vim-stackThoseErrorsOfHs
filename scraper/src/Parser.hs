module Parser 
( convertStackOutput
)
where

data ParseState = ParseState

convertStackOutput :: String -> String
convertStackOutput allInput = undefined $ foldr processLine ParseState $ lines allInput
  where
  processLine :: String -> ParseState -> ParseState
  processLine line = id
