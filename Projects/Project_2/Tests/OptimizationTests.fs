namespace Tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

open GuardedCommands.Util
open GuardedCommands.Frontend.TypeCheck
open GuardedCommands.Frontend.AST
open GuardedCommands.Backend.CodeGeneration
open GuardedCommands.Backend

open ParserUtil
open CompilerUtil
open Machine

[<TestClass>]
type TestOptimizations () =

    let verifyDeadCodeOptimization given expected =
        Assert.AreEqual(expected, DeadCodeAnalyzer.deadCodeOptimization given)

    [<TestMethod>]
    member this.DeadCode1 () =
        verifyDeadCodeOptimization [CSTI 1; CSTI 3; ADD; GOTO "32"; GOTO "r"; GETBP; LDI; Label "32"] [CSTI 1; CSTI 3; ADD]

    [<TestMethod>]
    member this.DeadCode2 () =
        verifyDeadCodeOptimization [CSTI 1; CSTI 3; ADD; GOTO "32"; GOTO "r"; GETBP; LDI; Label "32"] [CSTI 1; CSTI 3; ADD]    