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
        let actual = DeadCodeAnalyzer.deadCodeOptimization given
        Assert.AreEqual(expected, actual, String.Format("\nexpected:\n{0}\n\nbut got\n{1}", instrsToString expected, instrsToString actual))

    let verifyBranchOptimization given expected =
        let actual = BranchAnalyzer.optimizeBranching given
        Assert.AreEqual(expected, actual, String.Format("\nexpected:\n{0}\n\nbut got\n{1}", instrsToString expected, instrsToString actual))

    let verifyOptimization given expected =
        let actual = Optimizer.optimize given
        Assert.AreEqual(expected, actual, String.Format("\nexpected:\n{0}\n\nbut got\n{1}", instrsToString expected, instrsToString actual))    


    [<TestMethod>]
    member this.DeadCode1 () =
        verifyDeadCodeOptimization [CSTI 1; CSTI 3; ADD; GOTO "32"; GOTO "r"; GETBP; LDI; Label "32"] [CSTI 1; CSTI 3; ADD; GOTO "32"; Label "32"]

    [<TestMethod>]
    member this.DeadCode2 () =
        verifyDeadCodeOptimization [CSTI 1; CSTI 3; ADD; Label "3"; GOTO "32"; GOTO "r"; GETBP; LDI; Label "32"] [CSTI 1; CSTI 3; ADD; GOTO "32"; Label "32"]

    [<TestMethod>]
    member this.DeadCode3 () =
        verifyDeadCodeOptimization [CSTI 1; CSTI 3; ADD; RET 3; GOTO "r"; GETBP; LDI; Label "32"] [CSTI 1; CSTI 3; ADD; RET 3]

    [<TestMethod>]
    member this.DeadCode4 () =
        verifyDeadCodeOptimization [CSTI 1; CSTI 3; ADD; Label "3"; RET 3; GOTO "r"; GETBP; LDI; Label "32"] [CSTI 1; CSTI 3; ADD; RET 3]   

    [<TestMethod>]
    member this.BranchOptimizer1 () =
        verifyBranchOptimization [GOTO "1"; Label "1"; GOTO "2"; Label "2"] [GOTO "2"; Label "2"]

    [<TestMethod>]
    member this.BranchOptimizer2 () =
        verifyBranchOptimization [IFZERO "1"; Label "1"; GOTO "2"; Label "2"] [IFZERO "2"; Label "2"]

    [<TestMethod>]
    member this.BranchOptimizer3 () =
        verifyBranchOptimization [IFNZRO "1"; Label "1"; GOTO "2"; Label "2"] [IFNZRO "2"; Label "2"]

    [<TestMethod>]
    member this.BranchOptimizer4 () =
        verifyBranchOptimization [CALL(1, "1"); Label "1"; GOTO "2"; Label "2"] [CALL(1, "2"); Label "2"]

    [<TestMethod>]
    member this.BranchOptimizer5 () =
        verifyBranchOptimization [TCALL(1, 2, "1"); Label "1"; GOTO "2"; Label "2"] [TCALL(1, 2, "2"); Label "2"]         