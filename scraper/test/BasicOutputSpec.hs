{-# Language OverloadedStrings #-}

module BasicOutputSpec
( spec
)
where

import Prelude hiding (unlines, lines)
import Test.Hspec
import Parser
import Data.Text (Text, intercalate, unlines, lines)

spec :: Spec
spec = do 
  basicTest

basicTest :: Spec
basicTest = describe "basic output with one error gets parsed" $
  it "produces 1 output line" $
    lines (convertStackOutput basicError) `shouldBe` [
        "/path/project/src/Incremental/Workspaces.hs:470:37:error:Variable not in scope:"
      ]

basicError :: Text
basicError = 
  intercalate "\n" [ "project> build (lib)"
  , "Generating ResourceTRACS files..."
  , "Done!"
  , "Preprocessing library for project-0.0.0.."
  , "Building library for project-0.0.0.."
  , "[ 975 of 1029] Compiling Incremental.Workspaces [Source file changed]"
  , ""
  , "/path/project/src/Incremental/Workspaces.hs:470:37: error:"
  , "    Variable not in scope:"
  , "      (<|>)"
  , "        :: Maybe CrewDiag.CrewDiagram"
  , "           -> Maybe CrewDiag.CrewDiagram -> Maybe CrewDiag.CrewDiagram"
  , "    Suggested fix:"
  , "      Perhaps use one of these:"
  , "        ‘<>’ (imported from Prelude), ‘<$>’ (imported from Prelude),"
  , "        ‘<*>’ (imported from Prelude)"
  , "    |"
  , "470 |   getLightDiagram crewDiagId scheme <|> (getDiagramForValidationFromShadow =<< getShadowDiagram crewDiagId scheme)"
  , "    |                                     ^^^"
  , ""
  , "Error: [S-7282]"
  , "       Stack failed to execute the build plan."
  , "       "
  , "       While executing the build plan, Stack encountered the error:"
  , "       "
  , "       [S-7011]"
  , "       While building package project-0.0.0 (scroll up to its section to see the error) using:"
  , "       /path/project/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.8.1.0/setup/setup --verbose=1 --builddir=.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.8.1.0 build lib:project --ghc-options \" -fdiagnostics-color=always\""
  , "       Process exited with code: ExitFailure 1 "
  , "Type help for the available commands. Press enter to force a rebuild."
  ]
