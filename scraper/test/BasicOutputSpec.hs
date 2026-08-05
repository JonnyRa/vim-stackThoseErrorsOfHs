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
import Data.Text (Text, lines)

spec :: Spec
spec = describe "input examples" $ do 

  it "basic output with one error gets parsed produces 1 output line" $
    let input = [r|
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
    in
    expectOutput input [
        "/path/project/src/Incremental/Workspaces.hs:470:37:error:Variable not in scope: (<|>) :: Maybe CrewDiag.CrewDiagram -> Maybe CrewDiag.CrewDiagram -> Maybe CrewDiag.CrewDiagram Suggested fix: Perhaps use one of these: ‘<>’ (imported from Prelude), ‘<$>’ (imported from Prelude), ‘<*>’ (imported from Prelude)"
      ]

  it "multiple errors - errors are output in the right order" $ 
    let input = [r|
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
    in
    expectOutput input [
        "/path/project/src/Incremental/Workspaces.hs:470:37:error:Variable not in scope: (<|>) :: Maybe CrewDiag.CrewDiagram -> Maybe CrewDiag.CrewDiagram -> Maybe CrewDiag.CrewDiagram Suggested fix: Perhaps use one of these: ‘<>’ (imported from Prelude), ‘<$>’ (imported from Prelude), ‘<*>’ (imported from Prelude)"
      , "/path/project/src/Incremental/Workspaces.hs:536:14:error:Variable not in scope: fromMaybe :: [a1] -> Maybe [ValidationItem] -> [ValidationItem] Suggested fixes: • Perhaps use ‘Strict.fromMaybe’ (imported from Data.Strict) • Perhaps you want to add ‘fromMaybe’ to the import list in the import of ‘Data.Maybe’ (src/Incremental/Workspaces.hs:43:1-65)."
      ]

  it "basic interleaved output - can process well ordered errors with module prefixes" $
    let input = [r|
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
    in
    expectOutput input [
        "/path/src/Test/ModelHelper.hs:121:3:error:Illegal use of punning for field ‘modelSettingsOldValidation’ Suggested fix: Perhaps you intended to use NamedFieldPuns"
      ]

  it "basic interleaved output with no space after project name - can process errors for module prefixes that have no space before `>`" $
    let input = [r|
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
    in
    expectOutput input [
        "/path/project/selenium/SeleniumTests/CrewDiagramming/DiagramEditor/Skills.hs:11:51:error:Module ‘Test.Planning.Units.TestCode’ does not export ‘execPlanningFromArgs’"
      ]

  it "type signature in uninterleaved output - the error message isn't truncated because of the `>` in the type signature" $
    let input = [r|
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
    in
    expectOutput input [
        "/path/src/Handler/Tim/Api.hs:136:1:error:• Couldn't match type ‘Maybe Text -> FormationIssues’ with ‘FormationIssues’ Expected: Servant.Server.Internal.ServerT TrainFormationIssuesAPI ServantApiReader Actual: Date -> Maybe TrainExceptionMap -> ReaderT ApiConfig Servant.Server.Internal.Handler.Handler (Maybe Text -> FormationIssues) • The equation for ‘getTrainFormationIssuesApi’ has three value arguments, but its type ‘ServantApiEnv -> Servant.Server.Internal.ServerT TrainFormationIssuesAPI ServantApiReader’ has only one"
      ]

  it "long error messages - messages spread over multiple lines are collected and concatenated" $
    let input = [r|
/path.hs:32:46: warning: [GHC-38856] [-Wunused-imports]
    The import of ‘makeOvernightTurnaroundDirt’
    from module ‘Model.Control.MakeVexDirt’ is redundant
   |
32 |                                              makeOvernightTurnaroundDirt, makeRestrictionDirt,
   |                                              ^^^^^^^^^^^^^^^^^^^^^^^^^^^

/path.hs:72:50: error: [GHC-88464]
    Data constructor not in scope:
      AllDiry :: DirtyIdSet VehicleDiagramId
    Suggested fix:
      Perhaps use ‘AllDirty’ (imported from Types.Cacheing)
   |
72 |   makeRouteRestrictionViolationExceptionDirt d s AllDiry
   |                                                  ^^^^^^^|]
    in
    expectOutput input [
        "/path.hs:32:46:warning: [GHC-38856] [-Wunused-imports]The import of ‘makeOvernightTurnaroundDirt’ from module ‘Model.Control.MakeVexDirt’ is redundant"
      , "/path.hs:72:50:error: [GHC-88464]Data constructor not in scope: AllDiry :: DirtyIdSet VehicleDiagramId Suggested fix: Perhaps use ‘AllDirty’ (imported from Types.Cacheing)"
      ]

  --note this is a different kind of interleaved than when multiple packages are building at once
  it "interleaved output is stripped from error details" $
    let input = [r|
/path.hs:21:60: error: [GHC-61689]
[807 of 873] Compiling Handler.Rosters.Gantt [Foundation.App changed]
    Module ‘Types.Control.ControlModel’ does not export ‘vexCache’.
[808 of 873] Compiling Handler.Leave.Helper.Base [Foundation.App changed]
   |
[809 of 873] Compiling Handler.Leave.Helper.Types [Handler.Leave.Helper.Base changed]
21 | import Types.Control.ControlModel        (getOnTheDayWork, vexCache)
[810 of 873] Compiling Handler.Leave.Helper.Widget [Foundation.YesodData changed]
   ||]

    in
    expectOutput input [
        "/path.hs:21:60:error: [GHC-61689]Module ‘Types.Control.ControlModel’ does not export ‘vexCache’."
      ]

  it "c warnings don't screw up subsequent output" $
    let input = [r|
godot-haskell> build (lib + sub-lib + exe) with ghc-8.10.3
Preprocessing executable 'godot-haskell-project-generator' for godot-haskell-3.4.4.0..
Building executable 'godot-haskell-project-generator' for godot-haskell-3.4.4.0..
Preprocessing library 'generate' for godot-haskell-3.4.4.0..
Building library 'generate' for godot-haskell-3.4.4.0..
Preprocessing library for godot-haskell-3.4.4.0..
Building library for godot-haskell-3.4.4.0..
[  4 of 670] Compiling Godot.Gdnative.Internal.Types
[  5 of 670] Compiling Godot.Gdnative.Internal [Godot.Gdnative.Internal.Types changed]
[  6 of 670] Compiling Godot.Gdnative [Godot.Gdnative.Internal.Types changed]
[  7 of 670] Compiling Godot.Internal.Dispatch [Godot.Gdnative.Internal.Types changed]
[  8 of 670] Compiling Godot.Api.Types [Godot.Gdnative.Internal.Types changed]
/usr/bin/ld.bfd: warning: type and size of dynamic symbol `godotzmhaskellzm3zi4zi4zi0zm4h0vnMArnPkC0eWHkdyEFv_GodotziGdnativeziInternalziGdnative_zdfOpaqueStorableTYPEVector3_closure' are not defined
/usr/bin/ld.bfd: warning: type and size of dynamic symbol `godotzmhaskellzm3zi4zi4zi0zm4h0vnMArnPkC0eWHkdyEFv_GodotziGdnativeziInternalziGdnative_zdfOpaqueStorableTYPEPoolRealArray_closure' are not defined
/usr/bin/ld.bfd: warning: type and size of dynamic symbol `godotzmhaskellzm3zi4zi4zi0zm4h0vnMArnPkC0eWHkdyEFv_GodotziGdnativeziInternalziGdnative_zdfOpaqueStorableTYPECharString_closure' are not defined
/usr/bin/ld.bfd: warning: type and size of dynamic symbol `godotzmhaskellzm3zi4zi4zi0zm4h0vnMArnPkC0eWHkdyEFv_GodotziGdnativeziInternalziGdnative_zdfOpaqueStorableTYPEPlane_closure' are not defined
/usr/bin/ld.bfd: warning: type and size of dynamic symbol `godotzmhaskellzm3zi4zi4zi0zm4h0vnMArnPkC0eWHkdyEFv_GodotziGdnativeziInternalziGdnative_zdfOpaqueStorableTYPEPoolByteArray_closure' are not defined
/usr/bin/ld.bfd: warning: type and size of dynamic symbol `godotzmhaskellzm3zi4zi4zi0zm4h0vnMArnPkC0eWHkdyEFv_GodotziGdnativeziInternalziGdnative_zdfOpaqueStorableTYPEArray_closure' are not defined
/usr/bin/ld.bfd: warning: type and size of dynamic symbol `godotzmhaskellzm3zi4zi4zi0zm4h0vnMArnPkC0eWHkdyEFv_GodotziGdnativeziInternalziGdnative_zdfOpaqueStorableTYPEGodotVariant_closure' are not defined
/usr/bin/ld.bfd: warning: type and size of dynamic symbol `godotzmhaskellzm3zi4zi4zi0zm4h0vnMArnPkC0eWHkdyEFv_GodotziGdnativeziInternalziGdnative_zdfOpaqueStorableTYPEVector2_closure' are not defined
/usr/bin/ld.bfd: warning: type and size of dynamic symbol `godotzmhaskellzm3zi4zi4zi0zm4h0vnMArnPkC0eWHkdyEFv_GodotziGdnativeziInternalziGdnative_zdfOpaqueStorableTYPERid_closure' are not defined
/usr/bin/ld.bfd: warning: type and size of dynamic symbol `godotzmhaskellzm3zi4zi4zi0zm4h0vnMArnPkC0eWHkdyEFv_GodotziGdnativeziInternalziGdnative_zdfOpaqueStorableTYPETransform_closure' are not defined
/usr/bin/ld.bfd: warning: type and size of dynamic symbol `godotzmhaskellzm3zi4zi4zi0zm4h0vnMArnPkC0eWHkdyEFv_GodotziGdnativeziInternalziGdnative_zdfOpaqueStorableTYPEAabb_closure' are not defined
/usr/bin/ld.bfd: warning: type and size of dynamic symbol `godotzmhaskellzm3zi4zi4zi0zm4h0vnMArnPkC0eWHkdyEFv_GodotziGdnativeziInternalziGdnative_zdfOpaqueStorableTYPEGodotString_closure' are not defined
/usr/bin/ld.bfd: warning: type and size of dynamic symbol `godotzmhaskellzm3zi4zi4zi0zm4h0vnMArnPkC0eWHkdyEFv_GodotziGdnativeziInternalziGdnative_zdfOpaqueStorableTYPEColor_closure' are not defined
/usr/bin/ld.bfd: warning: type and size of dynamic symbol `godotzmhaskellzm3zi4zi4zi0zm4h0vnMArnPkC0eWHkdyEFv_GodotziGdnativeziInternalziGdnative_zdfOpaqueStorableTYPEPoolColorArray_closure' are not defined
/usr/bin/ld.bfd: warning: type and size of dynamic symbol `godotzmhaskellzm3zi4zi4zi0zm4h0vnMArnPkC0eWHkdyEFv_GodotziGdnativeziInternalziGdnative_zdfOpaqueStorableTYPEBasis_closure' are not defined
/usr/bin/ld.bfd: warning: type and size of dynamic symbol `godotzmhaskellzm3zi4zi4zi0zm4h0vnMArnPkC0eWHkdyEFv_GodotziGdnativeziInternalziGdnative_zdfOpaqueStorableTYPEPoolIntArray_closure' are not defined
/usr/bin/ld.bfd: warning: type and size of dynamic symbol `godotzmhaskellzm3zi4zi4zi0zm4h0vnMArnPkC0eWHkdyEFv_GodotziGdnativeziInternalziGdnative_zdfOpaqueStorableTYPEPoolVector2Array_closure' are not defined
/usr/bin/ld.bfd: warning: type and size of dynamic symbol `godotzmhaskellzm3zi4zi4zi0zm4h0vnMArnPkC0eWHkdyEFv_GodotziGdnativeziInternalziGdnative_zdfOpaqueStorableTYPEPoolVector3Array_closure' are not defined
/usr/bin/ld.bfd: warning: type and size of dynamic symbol `godotzmhaskellzm3zi4zi4zi0zm4h0vnMArnPkC0eWHkdyEFv_GodotziGdnativeziInternalziGdnative_zdfOpaqueStorableTYPEQuat_closure' are not defined
/usr/bin/ld.bfd: warning: type and size of dynamic symbol `godotzmhaskellzm3zi4zi4zi0zm4h0vnMArnPkC0eWHkdyEFv_GodotziGdnativeziInternalziGdnative_zdfOpaqueStorableTYPEStringName_closure' are not defined
/usr/bin/ld.bfd: warning: type and size of dynamic symbol `godotzmhaskellzm3zi4zi4zi0zm4h0vnMArnPkC0eWHkdyEFv_GodotziGdnativeziInternalziGdnative_zdfOpaqueStorableTYPEPoolStringArray_closure' are not defined
/usr/bin/ld.bfd: warning: type and size of dynamic symbol `godotzmhaskellzm3zi4zi4zi0zm4h0vnMArnPkC0eWHkdyEFv_GodotziGdnativeziInternalziGdnative_zdfOpaqueStorableTYPENodePath_closure' are not defined
/usr/bin/ld.bfd: warning: type and size of dynamic symbol `godotzmhaskellzm3zi4zi4zi0zm4h0vnMArnPkC0eWHkdyEFv_GodotziGdnativeziInternalziGdnative_zdfOpaqueStorableTYPERect2_closure' are not defined
/usr/bin/ld.bfd: warning: type and size of dynamic symbol `godotzmhaskellzm3zi4zi4zi0zm4h0vnMArnPkC0eWHkdyEFv_GodotziGdnativeziInternalziGdnative_zdfOpaqueStorableTYPETransform2d_closure' are not defined
/usr/bin/ld.bfd: warning: type and size of dynamic symbol `godotzmhaskellzm3zi4zi4zi0zm4h0vnMArnPkC0eWHkdyEFv_GodotziGdnativeziInternalziGdnative_zdfOpaqueStorableTYPEDictionary_closure' are not defined
[  9 of 670] Compiling Godot.Core.Object [Godot.Api.Types changed]
[ 10 of 670] Compiling Godot.Core.VisualServer [Godot.Api.Types changed]
[ 11 of 670] Compiling Godot.Core.UndoRedo [Godot.Api.Types changed]
[ 12 of 670] Compiling Godot.Core.TreeItem [Godot.Api.Types changed]
[ 13 of 670] Compiling Godot.Core.TranslationServer [Godot.Api.Types changed]
[ 14 of 670] Compiling Godot.Core.ResourceSaver [Godot.Api.Types changed]
[ 15 of 670] Compiling Godot.Core.ResourceLoader [Godot.Api.Types changed]
[ 16 of 670] Compiling Godot.Core.Reference [Godot.Api.Types changed]
[ 17 of 670] Compiling Godot.Core.XMLParser [Godot.Api.Types changed]
[ 18 of 670] Compiling Godot.Core.WebRTCPeerConnection [Godot.Api.Types changed]
[ 19 of 670] Compiling Godot.Core.WebRTCPeerConnectionGDNative [Godot.Core.WebRTCPeerConnection changed]
[ 20 of 670] Compiling Godot.Core.WeakRef [Godot.Api.Types changed]
[ 21 of 670] Compiling Godot.Core.VisualScriptFunctionState [Godot.Api.Types changed]
[ 22 of 670] Compiling Godot.Core.UPNPDevice [Godot.Api.Types changed]
[ 23 of 670] Compiling Godot.Core.UPNP [Godot.Api.Types changed]
[ 24 of 670] Compiling Godot.Core.UDPServer [Godot.Api.Types changed]
[ 25 of 670] Compiling Godot.Core.TriangleMesh [Godot.Core.Reference changed]
[ 26 of 670] Compiling Godot.Core.Thread [Godot.Api.Types changed]
[ 27 of 670] Compiling Godot.Core.TCP_Server [Godot.Api.Types changed]
[ 28 of 670] Compiling Godot.Core.SurfaceTool [Godot.Api.Types changed]
[ 29 of 670] Compiling Godot.Core.StreamPeer [Godot.Api.Types changed]
[ 30 of 670] Compiling Godot.Core.StreamPeerTCP [Godot.Api.Types changed]
[ 31 of 670] Compiling Godot.Core.StreamPeerSSL [Godot.Api.Types changed]
[ 32 of 670] Compiling Godot.Core.StreamPeerGDNative [Godot.Core.StreamPeer changed]
[ 33 of 670] Compiling Godot.Core.StreamPeerBuffer [Godot.Api.Types changed]
[ 34 of 670] Compiling Godot.Core.SpatialVelocityTracker [Godot.Api.Types changed]
[ 35 of 670] Compiling Godot.Core.SpatialGizmo [Godot.Core.Reference changed]
[ 36 of 670] Compiling Godot.Core.SkinReference [Godot.Api.Types changed]
[ 37 of 670] Compiling Godot.Core.Semaphore [Godot.Api.Types changed]
[ 38 of 670] Compiling Godot.Core.SceneTreeTimer [Godot.Api.Types changed]
[ 39 of 670] Compiling Godot.Core.SceneState [Godot.Api.Types changed]
[ 40 of 670] Compiling Godot.Core.ResourceInteractiveLoader [Godot.Api.Types changed]
[ 41 of 670] Compiling Godot.Core.ResourceImporter [Godot.Core.Reference changed]
[ 42 of 670] Compiling Godot.Core.ResourceFormatSaver [Godot.Api.Types changed]
[ 43 of 670] Compiling Godot.Core.ResourceFormatLoader [Godot.Api.Types changed]
[ 44 of 670] Compiling Godot.Core.Resource [Godot.Api.Types changed]
[ 45 of 670] Compiling Godot.Core.X509Certificate [Godot.Api.Types changed]
[ 46 of 670] Compiling Godot.Core.World2D [Godot.Api.Types changed]
[ 47 of 670] Compiling Godot.Core.World [Godot.Api.Types changed]
[ 48 of 670] Compiling Godot.Core.VisualShaderNode [Godot.Api.Types changed]
[ 49 of 670] Compiling Godot.Core.VisualShaderNodeVectorSmoothStep [Godot.Core.VisualShaderNode changed]
[ 50 of 670] Compiling Godot.Core.VisualShaderNodeVectorScalarStep [Godot.Core.VisualShaderNode changed]
[ 51 of 670] Compiling Godot.Core.VisualShaderNodeVectorScalarSmoothStep [Godot.Core.VisualShaderNode changed]
[ 52 of 670] Compiling Godot.Core.VisualShaderNodeVectorScalarMix [Godot.Core.VisualShaderNode changed]
[ 53 of 670] Compiling Godot.Core.VisualShaderNodeVectorRefract [Godot.Core.VisualShaderNode changed]
[ 54 of 670] Compiling Godot.Core.VisualShaderNodeVectorOp [Godot.Api.Types changed]
[ 55 of 670] Compiling Godot.Core.VisualShaderNodeVectorLen [Godot.Core.VisualShaderNode changed]
[ 56 of 670] Compiling Godot.Core.VisualShaderNodeVectorInterp [Godot.Core.VisualShaderNode changed]
[ 57 of 670] Compiling Godot.Core.VisualShaderNodeVectorFunc [Godot.Api.Types changed]
[ 58 of 670] Compiling Godot.Core.VisualShaderNodeVectorDistance [Godot.Core.VisualShaderNode changed]
[ 59 of 670] Compiling Godot.Core.VisualShaderNodeVectorDerivativeFunc [Godot.Api.Types changed]
[ 60 of 670] Compiling Godot.Core.VisualShaderNodeVectorDecompose [Godot.Core.VisualShaderNode changed]
[ 61 of 670] Compiling Godot.Core.VisualShaderNodeVectorCompose [Godot.Core.VisualShaderNode changed]
[ 62 of 670] Compiling Godot.Core.VisualShaderNodeVectorClamp [Godot.Core.VisualShaderNode changed]
[ 63 of 670] Compiling Godot.Core.VisualShaderNodeVec3Constant [Godot.Api.Types changed]
[ 64 of 670] Compiling Godot.Core.VisualShaderNodeUniformRef [Godot.Api.Types changed]
[ 65 of 670] Compiling Godot.Core.VisualShaderNodeUniform [Godot.Api.Types changed]
[ 66 of 670] Compiling Godot.Core.VisualShaderNodeVec3Uniform [Godot.Core.VisualShaderNodeUniform changed]
[ 67 of 670] Compiling Godot.Core.VisualShaderNodeTransformUniform [Godot.Core.VisualShaderNodeUniform changed]
[ 68 of 670] Compiling Godot.Core.VisualShaderNodeTextureUniform [Godot.Api.Types changed]
[ 69 of 670] Compiling Godot.Core.VisualShaderNodeTextureUniformTriplanar [Godot.Core.VisualShaderNodeTextureUniform changed]
[ 70 of 670] Compiling Godot.Core.VisualShaderNodeCubeMapUniform [Godot.Core.VisualShaderNodeTextureUniform changed]
[ 71 of 670] Compiling Godot.Core.VisualShaderNodeScalarUniform [Godot.Core.VisualShaderNodeUniform changed]
[ 72 of 670] Compiling Godot.Core.VisualShaderNodeColorUniform [Godot.Core.VisualShaderNodeUniform changed]
[ 73 of 670] Compiling Godot.Core.VisualShaderNodeBooleanUniform [Godot.Core.VisualShaderNodeUniform changed]
[ 74 of 670] Compiling Godot.Core.VisualShaderNodeTransformVecMult [Godot.Api.Types changed]
[ 75 of 670] Compiling Godot.Core.VisualShaderNodeTransformMult [Godot.Api.Types changed]
[ 76 of 670] Compiling Godot.Core.VisualShaderNodeTransformFunc [Godot.Api.Types changed]
[ 77 of 670] Compiling Godot.Core.VisualShaderNodeTransformDecompose [Godot.Core.VisualShaderNode changed]
[ 78 of 670] Compiling Godot.Core.VisualShaderNodeTransformConstant [Godot.Api.Types changed]
[ 79 of 670] Compiling Godot.Core.VisualShaderNodeTransformCompose [Godot.Core.VisualShaderNode changed]
[ 80 of 670] Compiling Godot.Core.VisualShaderNodeTexture [Godot.Api.Types changed]
[ 81 of 670] Compiling Godot.Core.VisualShaderNodeSwitch [Godot.Core.VisualShaderNode changed]
[ 82 of 670] Compiling Godot.Core.VisualShaderNodeScalarSwitch [Godot.Core.VisualShaderNodeSwitch changed]
[ 83 of 670] Compiling Godot.Core.VisualShaderNodeScalarSmoothStep [Godot.Core.VisualShaderNode changed]
[ 84 of 670] Compiling Godot.Core.VisualShaderNodeScalarOp [Godot.Api.Types changed]
[ 85 of 670] Compiling Godot.Core.VisualShaderNodeScalarInterp [Godot.Core.VisualShaderNode changed]
[ 86 of 670] Compiling Godot.Core.VisualShaderNodeScalarFunc [Godot.Api.Types changed]
[ 87 of 670] Compiling Godot.Core.VisualShaderNodeScalarDerivativeFunc [Godot.Api.Types changed]
[ 88 of 670] Compiling Godot.Core.VisualShaderNodeScalarConstant [Godot.Api.Types changed]
[ 89 of 670] Compiling Godot.Core.VisualShaderNodeScalarClamp [Godot.Core.VisualShaderNode changed]
[ 90 of 670] Compiling Godot.Core.VisualShaderNodeOutput [Godot.Core.VisualShaderNode changed]
[ 91 of 670] Compiling Godot.Core.VisualShaderNodeOuterProduct [Godot.Core.VisualShaderNode changed]
[ 92 of 670] Compiling Godot.Core.VisualShaderNodeIs [Godot.Api.Types changed]
[ 93 of 670] Compiling Godot.Core.VisualShaderNodeInput [Godot.Api.Types changed]
[ 94 of 670] Compiling Godot.Core.VisualShaderNodeIf [Godot.Core.VisualShaderNode changed]
[ 95 of 670] Compiling Godot.Core.VisualShaderNodeGroupBase [Godot.Api.Types changed]
[ 96 of 670] Compiling Godot.Core.VisualShaderNodeExpression [Godot.Api.Types changed]
[ 97 of 670] Compiling Godot.Core.VisualShaderNodeGlobalExpression [Godot.Core.VisualShaderNodeExpression changed]
[ 98 of 670] Compiling Godot.Core.VisualShaderNodeFresnel [Godot.Core.VisualShaderNode changed]
[ 99 of 670] Compiling Godot.Core.VisualShaderNodeFaceForward [Godot.Core.VisualShaderNode changed]
[100 of 670] Compiling Godot.Core.VisualShaderNodeDotProduct [Godot.Core.VisualShaderNode changed]
[101 of 670] Compiling Godot.Core.VisualShaderNodeDeterminant [Godot.Core.VisualShaderNode changed]
[102 of 670] Compiling Godot.Core.VisualShaderNodeCustom [Godot.Api.Types changed]
[103 of 670] Compiling Godot.Core.VisualShaderNodeCubeMap [Godot.Api.Types changed]
[104 of 670] Compiling Godot.Core.VisualShaderNodeCompare [Godot.Api.Types changed]
[105 of 670] Compiling Godot.Core.VisualShaderNodeColorOp [Godot.Api.Types changed]
[106 of 670] Compiling Godot.Core.VisualShaderNodeColorFunc [Godot.Api.Types changed]
[107 of 670] Compiling Godot.Core.VisualShaderNodeColorConstant [Godot.Api.Types changed]
[108 of 670] Compiling Godot.Core.VisualShaderNodeBooleanConstant [Godot.Api.Types changed]
[109 of 670] Compiling Godot.Core.VisualScriptNode [Godot.Api.Types changed]
[110 of 670] Compiling Godot.Core.VisualScriptYieldSignal [Godot.Api.Types changed]
[111 of 670] Compiling Godot.Core.VisualScriptYield [Godot.Api.Types changed]
[112 of 670] Compiling Godot.Core.VisualScriptWhile [Godot.Core.VisualScriptNode changed]
[113 of 670] Compiling Godot.Core.VisualScriptVariableSet [Godot.Api.Types changed]
[114 of 670] Compiling Godot.Core.VisualScriptVariableGet [Godot.Api.Types changed]
[115 of 670] Compiling Godot.Core.VisualScriptTypeCast [Godot.Api.Types changed]
[116 of 670] Compiling Godot.Core.VisualScriptSwitch [Godot.Core.VisualScriptNode changed]
[117 of 670] Compiling Godot.Core.VisualScriptSubCall [Godot.Api.Types changed]
[118 of 670] Compiling Godot.Core.VisualScriptSequence [Godot.Api.Types changed]
[119 of 670] Compiling Godot.Core.VisualScriptSelf [Godot.Core.VisualScriptNode changed]
[120 of 670] Compiling Godot.Core.VisualScriptSelect [Godot.Api.Types changed]
[121 of 670] Compiling Godot.Core.VisualScriptSceneTree [Godot.Core.VisualScriptNode changed]
[122 of 670] Compiling Godot.Core.VisualScriptSceneNode [Godot.Api.Types changed]
[123 of 670] Compiling Godot.Core.VisualScriptReturn [Godot.Api.Types changed]
[124 of 670] Compiling Godot.Core.VisualScriptResourcePath [Godot.Api.Types changed]
[125 of 670] Compiling Godot.Core.VisualScriptPropertySet [Godot.Api.Types changed]
[126 of 670] Compiling Godot.Core.VisualScriptPropertyGet [Godot.Api.Types changed]
[127 of 670] Compiling Godot.Core.VisualScriptPreload [Godot.Api.Types changed]
[128 of 670] Compiling Godot.Core.VisualScriptOperator [Godot.Api.Types changed]
[129 of 670] Compiling Godot.Core.VisualScriptMathConstant [Godot.Api.Types changed]
[130 of 670] Compiling Godot.Core.VisualScriptLocalVarSet [Godot.Api.Types changed]
[131 of 670] Compiling Godot.Core.VisualScriptLocalVar [Godot.Api.Types changed]
[132 of 670] Compiling Godot.Core.VisualScriptLists [Godot.Api.Types changed]
[133 of 670] Compiling Godot.Core.VisualScriptComposeArray [Godot.Core.VisualScriptLists changed]
[134 of 670] Compiling Godot.Core.VisualScriptIterator [Godot.Core.VisualScriptNode changed]
[135 of 670] Compiling Godot.Core.VisualScriptInputAction [Godot.Api.Types changed]
[136 of 670] Compiling Godot.Core.VisualScriptIndexSet [Godot.Core.VisualScriptNode changed]
[137 of 670] Compiling Godot.Core.VisualScriptIndexGet [Godot.Core.VisualScriptNode changed]
[138 of 670] Compiling Godot.Core.VisualScriptGlobalConstant [Godot.Api.Types changed]
[139 of 670] Compiling Godot.Core.VisualScriptFunctionCall [Godot.Api.Types changed]
[140 of 670] Compiling Godot.Core.VisualScriptFunction [Godot.Core.VisualScriptNode changed]
[141 of 670] Compiling Godot.Core.VisualScriptExpression [Godot.Core.VisualScriptNode changed]
[142 of 670] Compiling Godot.Core.VisualScriptEngineSingleton [Godot.Api.Types changed]
[143 of 670] Compiling Godot.Core.VisualScriptEmitSignal [Godot.Api.Types changed]
[144 of 670] Compiling Godot.Core.VisualScriptDeconstruct [Godot.Api.Types changed]
[145 of 670] Compiling Godot.Core.VisualScriptCustomNode [Godot.Api.Types changed]
[146 of 670] Compiling Godot.Core.VisualScriptConstructor [Godot.Api.Types changed]
[147 of 670] Compiling Godot.Core.VisualScriptConstant [Godot.Api.Types changed]
[148 of 670] Compiling Godot.Core.VisualScriptCondition [Godot.Core.VisualScriptNode changed]
[149 of 670] Compiling Godot.Core.VisualScriptComment [Godot.Api.Types changed]
[150 of 670] Compiling Godot.Core.VisualScriptClassConstant [Godot.Api.Types changed]
[151 of 670] Compiling Godot.Core.VisualScriptBuiltinFunc [Godot.Api.Types changed]
[152 of 670] Compiling Godot.Core.VisualScriptBasicTypeConstant [Godot.Api.Types changed]
[153 of 670] Compiling Godot.Core.VideoStream [Godot.Core.Resource changed]
[154 of 670] Compiling Godot.Core.VideoStreamWebm [Godot.Api.Types changed]
[155 of 670] Compiling Godot.Core.VideoStreamTheora [Godot.Api.Types changed]
[156 of 670] Compiling Godot.Core.VideoStreamGDNative [Godot.Api.Types changed]
[157 of 670] Compiling Godot.Core.Translation [Godot.Api.Types changed]
[158 of 670] Compiling Godot.Core.PHashTranslation [Godot.Api.Types changed]
[159 of 670] Compiling Godot.Core.TileSet [Godot.Api.Types changed]
[160 of 670] Compiling Godot.Core.Theme [Godot.Api.Types changed]
[161 of 670] Compiling Godot.Core.TextureLayered [Godot.Api.Types changed]
[162 of 670] Compiling Godot.Core.TextureArray [Godot.Api.Types changed]
[163 of 670] Compiling Godot.Core.Texture3D [Godot.Api.Types changed]
[164 of 670] Compiling Godot.Core.Texture [Godot.Api.Types changed]
[165 of 670] Compiling Godot.Core.ViewportTexture [Godot.Api.Types changed]
[166 of 670] Compiling Godot.Core.StreamTexture [Godot.Api.Types changed]
[167 of 670] Compiling Godot.Core.ProxyTexture [Godot.Api.Types changed]
[168 of 670] Compiling Godot.Core.TextFile [Godot.Core.Resource changed]
[169 of 670] Compiling Godot.Core.StyleBox [Godot.Api.Types changed]
[170 of 670] Compiling Godot.Core.StyleBoxTexture [Godot.Api.Types changed]
[171 of 670] Compiling Godot.Core.StyleBoxLine [Godot.Api.Types changed]
[172 of 670] Compiling Godot.Core.StyleBoxFlat [Godot.Api.Types changed]
[173 of 670] Compiling Godot.Core.StyleBoxEmpty [Godot.Core.StyleBox changed]
[174 of 670] Compiling Godot.Core.SpriteFrames [Godot.Api.Types changed]
[175 of 670] Compiling Godot.Core.Sky [Godot.Api.Types changed]
[176 of 670] Compiling Godot.Core.ProceduralSky [Godot.Api.Types changed]
[177 of 670] Compiling Godot.Core.PanoramaSky [Godot.Api.Types changed]
[178 of 670] Compiling Godot.Core.Skin [Godot.Api.Types changed]
[179 of 670] Compiling Godot.Core.ShortCut [Godot.Api.Types changed]
[180 of 670] Compiling Godot.Core.Shape2D [Godot.Api.Types changed]
[181 of 670] Compiling Godot.Core.SegmentShape2D [Godot.Api.Types changed]
[182 of 670] Compiling Godot.Core.RectangleShape2D [Godot.Api.Types changed]
[183 of 670] Compiling Godot.Core.RayShape2D [Godot.Api.Types changed]
[184 of 670] Compiling Godot.Core.Shape [Godot.Api.Types changed]
[185 of 670] Compiling Godot.Core.SphereShape [Godot.Api.Types changed]
[186 of 670] Compiling Godot.Core.RayShape [Godot.Api.Types changed]
[187 of 670] Compiling Godot.Core.PlaneShape [Godot.Api.Types changed]
[188 of 670] Compiling Godot.Core.Shader [Godot.Api.Types changed]
[189 of 670] Compiling Godot.Core.VisualShader [Godot.Api.Types changed]
[190 of 670] Compiling Godot.Core.Script [Godot.Api.Types changed]
[191 of 670] Compiling Godot.Core.VisualScript [Godot.Api.Types changed]
[192 of 670] Compiling Godot.Core.PluginScript [Godot.Api.Types changed]
[193 of 670] Compiling Godot.Core.RichTextEffect [Godot.Api.Types changed]
[194 of 670] Compiling Godot.Core.PolygonPathFinder [Godot.Api.Types changed]
[195 of 670] Compiling Godot.Core.PhysicsMaterial [Godot.Api.Types changed]
[196 of 670] Compiling Godot.Core.PackedScene [Godot.Api.Types changed]
[197 of 670] Compiling Godot.Core.PackedSceneGLTF [Godot.Api.Types changed]
[198 of 670] Compiling Godot.Core.PackedDataContainer [Godot.Api.Types changed]
[199 of 670] Compiling Godot.Core.OpenSimplexNoise [Godot.Api.Types changed]
[200 of 670] Compiling Godot.Core.OccluderShape [Godot.Core.Resource changed]
[201 of 670] Compiling Godot.Core.OccluderShapeSphere [Godot.Api.Types changed]
[202 of 670] Compiling Godot.Core.OccluderPolygon2D [Godot.Api.Types changed]
[203 of 670] Compiling Godot.Core.RegExMatch [Godot.Api.Types changed]
[204 of 670] Compiling Godot.Core.RegEx [Godot.Api.Types changed]
[205 of 670] Compiling Godot.Core.RandomNumberGenerator [Godot.Api.Types changed]
[206 of 670] Compiling Godot.Core.PhysicsTestMotionResult [Godot.Api.Types changed]
[207 of 670] Compiling Godot.Core.PhysicsShapeQueryParameters [Godot.Api.Types changed]
[208 of 670] Compiling Godot.Core.Physics2DTestMotionResult [Godot.Api.Types changed]
[209 of 670] Compiling Godot.Core.Physics2DShapeQueryParameters [Godot.Api.Types changed]
[210 of 670] Compiling Godot.Core.PacketPeer [Godot.Api.Types changed]
[211 of 670] Compiling Godot.Core.WebSocketPeer [Godot.Api.Types changed]
[212 of 670] Compiling Godot.Core.WebRTCDataChannel [Godot.Api.Types changed]
[213 of 670] Compiling Godot.Core.WebRTCDataChannelGDNative [Godot.Core.WebRTCDataChannel changed]
[214 of 670] Compiling Godot.Core.PacketPeerUDP [Godot.Api.Types changed]
[215 of 670] Compiling Godot.Core.PacketPeerStream [Godot.Api.Types changed]
[216 of 670] Compiling Godot.Core.PacketPeerGDNative [Godot.Core.PacketPeer changed]
[217 of 670] Compiling Godot.Core.PacketPeerDTLS [Godot.Api.Types changed]
[218 of 670] Compiling Godot.Core.PackedDataContainerRef [Godot.Api.Types changed]
[219 of 670] Compiling Godot.Core.PCKPacker [Godot.Api.Types changed]
[220 of 670] Compiling Godot.Core.ProjectSettings [Godot.Api.Types changed]
[221 of 670] Compiling Godot.Core.PhysicsServer [Godot.Api.Types changed]
[222 of 670] Compiling Godot.Core.PhysicsDirectSpaceState [Godot.Api.Types changed]
[223 of 670] Compiling Godot.Core.PhysicsDirectBodyState [Godot.Api.Types changed]
[224 of 670] Compiling Godot.Core.Physics2DServer [Godot.Api.Types changed]
[225 of 670] Compiling Godot.Core.Physics2DServerSW [Godot.Core.Physics2DServer changed]
[226 of 670] Compiling Godot.Core.Physics2DDirectSpaceState [Godot.Api.Types changed]
[227 of 670] Compiling Godot.Core.Physics2DDirectBodyState [Godot.Api.Types changed]
[228 of 670] Compiling Godot.Core.Physics2DDirectBodyStateSW [Godot.Core.Physics2DDirectBodyState changed]
[229 of 670] Compiling Godot.Core.Performance [Godot.Api.Types changed]
[230 of 670] Compiling Godot.Core.OS [Godot.Api.Types changed]
[231 of 670] Compiling Godot.Core.NoiseTexture [Godot.Api.Types changed]
[232 of 670] Compiling Godot.Core.Node [Godot.Api.Types changed]
[233 of 670] Compiling Godot.Core.WorldEnvironment [Godot.Api.Types changed]
[234 of 670] Compiling Godot.Core.Viewport [Godot.Api.Types changed]
[235 of 670] Compiling Godot.Core.Tween [Godot.Api.Types changed]
[236 of 670] Compiling Godot.Core.Timer [Godot.Api.Types changed]
[237 of 670] Compiling Godot.Core.Spatial [Godot.Api.Types changed]
[238 of 670] Compiling Godot.Core.VehicleWheel [Godot.Api.Types changed]
[239 of 670] Compiling Godot.Core.SpringArm [Godot.Api.Types changed]
[240 of 670] Compiling Godot.Core.Skeleton [Godot.Api.Types changed]
[241 of 670] Compiling Godot.Core.RoomManager [Godot.Api.Types changed]
[242 of 670] Compiling Godot.Core.RoomGroup [Godot.Api.Types changed]
[243 of 670] Compiling Godot.Core.Room [Godot.Api.Types changed]
[244 of 670] Compiling Godot.Core.RemoteTransform [Godot.Api.Types changed]
[245 of 670] Compiling Godot.Core.RayCast [Godot.Api.Types changed]
[246 of 670] Compiling Godot.Core.ProximityGroup [Godot.Api.Types changed]
[247 of 670] Compiling Godot.Core.Position3D [Godot.Core.Spatial changed]
[248 of 670] Compiling Godot.Core.Portal [Godot.Api.Types changed]
[249 of 670] Compiling Godot.Core.PathFollow [Godot.Api.Types changed]
[250 of 670] Compiling Godot.Core.Path [Godot.Api.Types changed]
[251 of 670] Compiling Godot.Core.Occluder [Godot.Api.Types changed]
[252 of 670] Compiling Godot.Core.SkeletonIK [Godot.Api.Types changed]
[253 of 670] Compiling Godot.Core.ResourcePreloader [Godot.Api.Types changed]
[254 of 670] Compiling Godot.Core.NetworkedMultiplayerPeer [Godot.Api.Types changed]
[255 of 670] Compiling Godot.Core.WebSocketMultiplayerPeer [Godot.Api.Types changed]
[256 of 670] Compiling Godot.Core.WebSocketServer [Godot.Api.Types changed]
[257 of 670] Compiling Godot.Core.WebSocketClient [Godot.Api.Types changed]
[258 of 670] Compiling Godot.Core.WebRTCMultiplayer [Godot.Api.Types changed]
[259 of 670] Compiling Godot.Core.NetworkedMultiplayerENet [Godot.Api.Types changed]
[260 of 670] Compiling Godot.Core.NavigationPolygon [Godot.Api.Types changed]
[261 of 670] Compiling Godot.Core.NavigationMeshInstance [Godot.Api.Types changed]
[262 of 670] Compiling Godot.Core.NavigationMesh [Godot.Api.Types changed]
[263 of 670] Compiling Godot.Core.Navigation [Godot.Api.Types changed]
[264 of 670] Compiling Godot.Core.NativeScript [Godot.Api.Types changed]
[265 of 670] Compiling Godot.Core.Mutex [Godot.Api.Types changed]
[266 of 670] Compiling Godot.Core.MultiplayerPeerGDNative [Godot.Core.NetworkedMultiplayerPeer changed]
[267 of 670] Compiling Godot.Core.MultiplayerAPI [Godot.Api.Types changed]
[268 of 670] Compiling Godot.Core.MultiMesh [Godot.Api.Types changed]
[269 of 670] Compiling Godot.Core.MeshTexture [Godot.Api.Types changed]
[270 of 670] Compiling Godot.Core.MeshLibrary [Godot.Api.Types changed]
[271 of 670] Compiling Godot.Core.MeshDataTool [Godot.Api.Types changed]
[272 of 670] Compiling Godot.Core.Mesh [Godot.Api.Types changed]
[273 of 670] Compiling Godot.Core.PrimitiveMesh [Godot.Api.Types changed]
[274 of 670] Compiling Godot.Core.SphereMesh [Godot.Api.Types changed]
[275 of 670] Compiling Godot.Core.QuadMesh [Godot.Api.Types changed]
[276 of 670] Compiling Godot.Core.PrismMesh [Godot.Api.Types changed]
[277 of 670] Compiling Godot.Core.PointMesh [Godot.Core.PrimitiveMesh changed]
[278 of 670] Compiling Godot.Core.PlaneMesh [Godot.Api.Types changed]
[279 of 670] Compiling Godot.Core.Material [Godot.Api.Types changed]
[280 of 670] Compiling Godot.Core.SpatialMaterial [Godot.Api.Types changed]
[281 of 670] Compiling Godot.Core.ShaderMaterial [Godot.Api.Types changed]
[282 of 670] Compiling Godot.Core.ParticlesMaterial [Godot.Api.Types changed]
[283 of 670] Compiling Godot.Core.Marshalls [Godot.Api.Types changed]
[284 of 670] Compiling Godot.Core.MainLoop [Godot.Api.Types changed]
[285 of 670] Compiling Godot.Core.SceneTree [Godot.Api.Types changed]
[286 of 670] Compiling Godot.Core.Listener [Godot.Api.Types changed]
[287 of 670] Compiling Godot.Core.LineShape2D [Godot.Api.Types changed]
[288 of 670] Compiling Godot.Core.LargeTexture [Godot.Api.Types changed]
[289 of 670] Compiling Godot.Core.KinematicCollision2D [Godot.Api.Types changed]
[290 of 670] Compiling Godot.Core.KinematicCollision [Godot.Api.Types changed]
[291 of 670] Compiling Godot.Core.Joint [Godot.Api.Types changed]
[292 of 670] Compiling Godot.Core.SliderJoint [Godot.Api.Types changed]
[293 of 670] Compiling Godot.Core.PinJoint [Godot.Api.Types changed]
[294 of 670] Compiling Godot.Core.JavaScriptObject [Godot.Core.Reference changed]
[295 of 670] Compiling Godot.Core.JavaScript [Godot.Api.Types changed]
[296 of 670] Compiling Godot.Core.JavaClassWrapper [Godot.Api.Types changed]
[297 of 670] Compiling Godot.Core.JavaClass [Godot.Core.Reference changed]
[298 of 670] Compiling Godot.Core.JSONRPC [Godot.Api.Types changed]
[299 of 670] Compiling Godot.Core.JSONParseResult [Godot.Api.Types changed]
[300 of 670] Compiling Godot.Core.JSON [Godot.Api.Types changed]
[301 of 670] Compiling Godot.Core.JNISingleton [Godot.Core.Object changed]
[302 of 670] Compiling Godot.Core.InstancePlaceholder [Godot.Api.Types changed]
[303 of 670] Compiling Godot.Core.InputMap [Godot.Api.Types changed]
[304 of 670] Compiling Godot.Core.InputEvent [Godot.Api.Types changed]
[305 of 670] Compiling Godot.Core.InputEventWithModifiers [Godot.Api.Types changed]
[306 of 670] Compiling Godot.Core.InputEventMouse [Godot.Api.Types changed]
[307 of 670] Compiling Godot.Core.InputEventMouseMotion [Godot.Api.Types changed]
[308 of 670] Compiling Godot.Core.InputEventMouseButton [Godot.Api.Types changed]
[309 of 670] Compiling Godot.Core.InputEventKey [Godot.Api.Types changed]
[310 of 670] Compiling Godot.Core.InputEventGesture [Godot.Api.Types changed]
[311 of 670] Compiling Godot.Core.InputEventPanGesture [Godot.Api.Types changed]
[312 of 670] Compiling Godot.Core.InputEventMagnifyGesture [Godot.Api.Types changed]
[313 of 670] Compiling Godot.Core.InputEventScreenTouch [Godot.Api.Types changed]
[314 of 670] Compiling Godot.Core.InputEventScreenDrag [Godot.Api.Types changed]
[315 of 670] Compiling Godot.Core.InputEventMIDI [Godot.Api.Types changed]
[316 of 670] Compiling Godot.Core.InputEventJoypadMotion [Godot.Api.Types changed]
[317 of 670] Compiling Godot.Core.InputEventJoypadButton [Godot.Api.Types changed]
[318 of 670] Compiling Godot.Core.InputEventAction [Godot.Api.Types changed]
[319 of 670] Compiling Godot.Core.Input [Godot.Api.Types changed]
[320 of 670] Compiling Godot.Core.InputDefault [Godot.Core.Input changed]
[321 of 670] Compiling Godot.Core.ImageTexture [Godot.Api.Types changed]
[322 of 670] Compiling Godot.Core.Image [Godot.Api.Types changed]
[323 of 670] Compiling Godot.Core.IP [Godot.Api.Types changed]
[324 of 670] Compiling Godot.Core.IP_Unix [Godot.Core.IP changed]
[325 of 670] Compiling Godot.Core.HingeJoint [Godot.Api.Types changed]
[326 of 670] Compiling Godot.Core.HeightMapShape [Godot.Api.Types changed]
[327 of 670] Compiling Godot.Core.HashingContext [Godot.Api.Types changed]
[328 of 670] Compiling Godot.Core.HTTPRequest
[329 of 670] Compiling Godot.Core.HTTPClient
[330 of 670] Compiling Godot.Core.HMACContext
[331 of 670] Compiling Godot.Core.GridMap
[332 of 670] Compiling Godot.Core.GradientTexture
[333 of 670] Compiling Godot.Core.Gradient
[334 of 670] Compiling Godot.Core.GlobalConstants
[335 of 670] Compiling Godot.Core.Geometry
[336 of 670] Compiling Godot.Core.Generic6DOFJoint
[337 of 670] Compiling Godot.Core.GLTFTexture
[338 of 670] Compiling Godot.Core.GLTFState

/home/jonny/repos/godot-haskell/src/Godot/Core/GLTFState.hs:116:14: error:
    • Couldn't match type ‘Array’ with ‘PoolIntArray’
      Expected type: GLTFState -> IO PoolIntArray
        Actual type: GLTFState -> IO Array
    • In the expression: get_root_nodes
      In the expression:
        (get_root_nodes, wrapDroppingSetter set_root_nodes, Nothing)
      In an equation for ‘nodeProperty’:
          nodeProperty
            = (get_root_nodes, wrapDroppingSetter set_root_nodes, Nothing)
    |
116 |           = (get_root_nodes, wrapDroppingSetter set_root_nodes, Nothing)
    |              ^^^^^^^^^^^^^^


Error: [S-7282]
       Stack failed to execute the build plan.

       While executing the build plan, Stack encountered the error:

       [S-7011]
       While building package godot-haskell-3.4.4.0 (scroll up to its section to see the error)
       using:
       /home/jonny/.stack/setup-exe-cache/x86_64-linux-tinfo6/Cabal-simple_w2MFVN35_3.2.1.0_ghc-8.10.3 --verbose=1 --builddir=.stack-work/dist/x86_64-linux-tinfo6/ghc-8.10.3 build lib:godot-haskell lib:generate exe:godot-haskell-project-generator --ghc-options " -fdiagnostics-color=always"
       Process exited with code: ExitFailure 1
Type help for the available commands. Press enter to force a rebuild.
   ||]

    in
    expectOutput input [
        "/path.hs:21:60:error: [GHC-61689]Module ‘Types.Control.ControlModel’ does not export ‘vexCache’."
      ]

expectOutput :: Text -> [Text] -> Expectation
expectOutput input expected =
    lines (convertStackOutput input) `shouldBe` expected
