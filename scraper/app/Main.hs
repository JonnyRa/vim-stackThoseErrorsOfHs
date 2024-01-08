module Main (main) where

import Parser
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  lazyContents <- readFile inputFile
  pure $ convertStackOutput lazyContents
  pure ()
