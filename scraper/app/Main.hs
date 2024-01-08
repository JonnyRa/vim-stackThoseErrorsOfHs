module Main (main) where

import Parser
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  let outputFile = last args
  lazyContents <- readFile inputFile
  let output = convertStackOutput lazyContents
  writeFile outputFile output
