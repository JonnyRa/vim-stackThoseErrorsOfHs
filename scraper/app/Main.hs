module Main (main) where

import Prelude hiding (readFile, writeFile)
import Parser
import System.Environment
import System.Exit
import Control.Monad
import Data.Text.IO (readFile, writeFile)

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 2) $ do
    putStrLn "Error needs to 2 arguments: inputFile outputFile"
    exitFailure

  let inputFile = head args
  let outputFile = last args
  lazyContents <- readFile inputFile
  let output = convertStackOutput lazyContents
  writeFile outputFile output
