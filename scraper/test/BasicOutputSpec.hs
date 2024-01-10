{-# Language OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module BasicOutputSpec
( spec
)
where

import Text.RawString.QQ
import Prelude hiding (unlines, lines)
import Test.Hspec
import Parser
import Data.Text (Text, intercalate, unlines, lines)

spec :: Spec
spec = do 
  basicTest
  twoErrorsTest

basicTest :: Spec
basicTest = describe "basic output with one error gets parsed" $
  it "produces 1 output line" $
    expectOutput basicError [
        "/path/project/src/Incremental/Workspaces.hs:470:37:error:Variable not in scope:"
      ]

basicError :: Text
basicError = [r|
project> build (lib)
Generating ResourceTRACS files...
Done!
Preprocessing library for project-0.0.0..
Building library for project-0.0.0..
[ 975 of 1029] Compiling Incremental.Workspaces [Source file changed]

/path/project/src/Incremental/Workspaces.hs:470:37: error:
    Variable not in scope:
      (<|>)
        :: Maybe CrewDiag.CrewDiagram
           -> Maybe CrewDiag.CrewDiagram -> Maybe CrewDiag.CrewDiagram
    Suggested fix:
      Perhaps use one of these:
        ‘<>’ (imported from Prelude), ‘<$>’ (imported from Prelude),
        ‘<*>’ (imported from Prelude)
    |
470 |   getLightDiagram crewDiagId scheme <|> (getDiagramForValidationFromShadow =<< getShadowDiagram crewDiagId scheme)
    |                                     ^^^

Error: [S-7282]
       Stack failed to execute the build plan.
       
       While executing the build plan, Stack encountered the error:
       
       [S-7011]
       While building package project-0.0.0 (scroll up to its section to see the error) using:
       /path/project/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.8.1.0/setup/setup --verbose=1 --builddir=.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.8.1.0 build lib:project --ghc-options \" -fdiagnostics-color=always\"
       Process exited with code: ExitFailure 1 
Type help for the available commands. Press enter to force a rebuild.|]



twoErrors :: Text
twoErrors = [r|
trent-model> build (lib)
Generating ResourceTRACS files...
Done!
Preprocessing library for trent-model-0.0.0..
Building library for trent-model-0.0.0..
[ 975 of 1029] Compiling Incremental.Workspaces [Source file changed]

/path/project/src/Incremental/Workspaces.hs:470:37: error:
    Variable not in scope:
      (<|>)
        :: Maybe CrewDiag.CrewDiagram
           -> Maybe CrewDiag.CrewDiagram -> Maybe CrewDiag.CrewDiagram
    Suggested fix:
      Perhaps use one of these:
        ‘<>’ (imported from Prelude), ‘<$>’ (imported from Prelude),
        ‘<*>’ (imported from Prelude)
    |
470 |   getLightDiagram crewDiagId scheme <|> (getDiagramForValidationFromShadow =<< getShadowDiagram crewDiagId scheme)
    |                                     ^^^

/path/project/src/Incremental/Workspaces.hs:536:14: error:
    Variable not in scope:
      fromMaybe :: [a1] -> Maybe [ValidationItem] -> [ValidationItem]
    Suggested fixes:
      • Perhaps use ‘Strict.fromMaybe’ (imported from Data.Strict)
      • Perhaps you want to add ‘fromMaybe’ to the import list
        in the import of ‘Data.Maybe’
        (src/Incremental/Workspaces.hs:43:1-65).
    |
536 |     local <- fromMaybe [] <$> readCacheMapVar crewDiagId valItems
    |              ^^^^^^^^^

Error: [S-7282]
       Stack failed to execute the build plan.
       
       While executing the build plan, Stack encountered the error:
       
       [S-7011]
       While building package trent-model-0.0.0 (scroll up to its section to see the error) using:
       /path/project/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.8.1.0/setup/setup --verbose=1 --builddir=.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.8.1.0 build lib:trent-model --ghc-options " -fdiagnostics-color=always"
       Process exited with code: ExitFailure 1 
Type help for the available commands. Press enter to force a rebuild.|]


twoErrorsTest :: Spec
twoErrorsTest = describe "multiple errors" $ 
  it "errors are output in the right order" $ 
    expectOutput twoErrors [
        "/path/project/src/Incremental/Workspaces.hs:470:37:error:Variable not in scope:"
      , "/path/project/src/Incremental/Workspaces.hs:536:14:error:Variable not in scope:"
      ]

expectOutput :: Text -> [Text] -> Expectation
expectOutput input expected =
    lines (convertStackOutput input) `shouldBe` expected
