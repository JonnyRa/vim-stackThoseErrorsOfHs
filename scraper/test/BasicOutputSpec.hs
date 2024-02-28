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
  projectPrefixedTest
  projectPrefixedNoSpaceTest
  typeSignatureInErrorMessageNotInterleavedTest

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
       /path/project/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.8.1.0/setup/setup --verbose=1 --builddir=.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.8.1.0 build lib:project --ghc-options " -fdiagnostics-color=always"
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

projectPrefixedErrors :: Text
projectPrefixedErrors = [r|
trent-testing         > configure (lib + exe)
trent-testing         > Configuring trent-testing-0.1.0.0...
trent-testing         > build (lib + exe)
trent-testing         > Preprocessing library for trent-testing-0.1.0.0..
trent-testing         > Building library for trent-testing-0.1.0.0..
trent-testing         > [16 of 71] Compiling Test.ModelHelper
trent-testing         > 
trent-testing         > /path/src/Test/ModelHelper.hs:121:3: error:
trent-testing         >     Illegal use of punning for field ‘modelSettingsOldValidation’
trent-testing         >     Suggested fix: Perhaps you intended to use NamedFieldPuns
trent-testing         >     |
trent-testing         > 121 |   ModelSettings
trent-testing         >     |   ^^^^^^^^^^^^^...

Error: [S-7282]
       Stack failed to execute the build plan.
       
       While executing the build plan, Stack encountered the error:
       
       [S-7011]
       While building package trent-testing-0.1.0.0 (scroll up to its section to see the error)
       using:
       /path/.stack/setup-exe-cache/x86_64-linux-tinfo6/Cabal-simple_6HauvNHV_3.8.1.0_ghc-9.4.7 --verbose=1 --builddir=.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.8.1.0 build lib:trent-testing exe:read-events exe:write-events --ghc-options " -fdiagnostics-color=always"
       Process exited with code: ExitFailure 1 
Type help for the available commands. Press enter to force a rebuild.|]

projectPrefixedTest :: Spec
projectPrefixedTest = describe "basic interleaved output" $
  it "can process well ordered errors with module prefixes" $
    expectOutput projectPrefixedErrors [
        "/path/src/Test/ModelHelper.hs:121:3:error:Illegal use of punning for field ‘modelSettingsOldValidation’"
      ]

projectPrefixedNoSpaceErrors :: Text
projectPrefixedNoSpaceErrors = [r|
a-longer-path> [432 of 810] Compiling TestUtils.ControlModelTracking [TestUtils package changed]
a-longer-path> [504 of 810] Compiling SeleniumTests.CrewDiagramming.DiagramEditor.Skills [Source file changed]
a-longer-path> /path/project/selenium/SeleniumTests/CrewDiagramming/DiagramEditor/Skills.hs:11:51: error:
a-longer-path>     Module
a-longer-path>     ‘Test.Planning.Units.TestCode’
a-longer-path>     does not export
a-longer-path>     ‘execPlanningFromArgs’
a-longer-path>    |
a-longer-path> 11 | import Test.Planning.Units.TestCode             ( execPlanningFromArgs, setupUnitPlan, applyEvents, releaseAllLTPUnits, setupDates,
a-longer-path>    |                                                   ^^^^^^^^^^^^^^^^^^^^
short-path   > Preprocessing library for trent-database-tests-0.1.0.0..
short-path   > Building library for trent-database-tests-0.1.0.0..
short-path   > copy/register

Type help for the available commands. Press enter to force a rebuild.|]

projectPrefixedNoSpaceTest :: Spec
projectPrefixedNoSpaceTest = describe "basic interleaved output with no space after project name" $
  it "can process errors for module prefixes that have no space before `>`" $
    expectOutput projectPrefixedNoSpaceErrors [
        "/path/project/selenium/SeleniumTests/CrewDiagramming/DiagramEditor/Skills.hs:11:51:error:Module"
      ]

typeSignatureInErrorMessageNotInterleaved :: Text
typeSignatureInErrorMessageNotInterleaved = [r|
/path/src/Handler/Tim/Api.hs:136:1: error:
    • Couldn't match type ‘Maybe Text -> FormationIssues’
                     with ‘FormationIssues’
      Expected: Servant.Server.Internal.ServerT
                  TrainFormationIssuesAPI ServantApiReader
        Actual: Date
                -> Maybe TrainExceptionMap
                -> ReaderT
                     ApiConfig
                     Servant.Server.Internal.Handler.Handler
                     (Maybe Text -> FormationIssues)
    • The equation for ‘getTrainFormationIssuesApi’ has three value arguments,
        but its type ‘ServantApiEnv
                      -> Servant.Server.Internal.ServerT
                           TrainFormationIssuesAPI ServantApiReader’
        has only one
    |
136 | getTrainFormationIssuesApi _env day mText = do
    | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^|]

typeSignatureInErrorMessageNotInterleavedTest :: Spec
typeSignatureInErrorMessageNotInterleavedTest = describe "type signature in uninterleaved output" $
  it "the error message isn't truncated because of the `>` in the type signature" $
    expectOutput typeSignatureInErrorMessageNotInterleaved [
        "/path/src/Handler/Tim/Api.hs:136:1:error:• Couldn't match type ‘Maybe Text -> FormationIssues’"
      ]

expectOutput :: Text -> [Text] -> Expectation
expectOutput input expected =
    lines (convertStackOutput input) `shouldBe` expected
