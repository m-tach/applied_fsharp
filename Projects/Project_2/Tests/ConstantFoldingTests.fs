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
    
    let verifyOptimization given expected =
        let actual = Optimizer.optimize given
        Assert.AreEqual(expected, actual, String.Format("\nexpected:\n{0}\n\nbut got\n{1}", instrsToString expected, instrsToString actual))

    let verifyOptimizationReturnsConstant given =
        match Optimizer.optimize given with
        | [CSTI _] -> ()
        | a -> Assert.Fail("expected a constant but got " + (a.ToString()))  

    [<TestMethod>]
    member this.ConstantFolding1 () =
        verifyOptimization [CSTI 1; CSTI 2; ADD; CSTI 3; CSTI 1; SUB; MUL] [CSTI 6]

    [<TestMethod>]
    member this.ConstantFolding2 () =
        verifyOptimization [CSTI 3; CSTI 4; ADD] [CSTI 7]  

    [<TestMethod>]
    member this.ConstantFolding3 () =
        verifyOptimization [CSTI 3; CSTI 4; SUB] [CSTI -1]

    [<TestMethod>]
    member this.ConstantFolding4 () =
        verifyOptimization [CSTI 3; CSTI 4; MUL] [CSTI 12]

    [<TestMethod>]
    member this.ConstantFolding5 () =
        verifyOptimization [CSTI 3; CSTI 4; DIV] [CSTI 0]

    [<TestMethod>]
    member this.ConstantFolding6 () =
        verifyOptimization [CSTI 3; CSTI 4; MOD] [CSTI 3]

    [<TestMethod>]
    member this.ConstantFolding7 () =
        verifyOptimization [CSTI 3; CSTI 4; EQ ] [CSTI 0]

    [<TestMethod>]
    member this.ConstantFolding8 () =
        verifyOptimization [CSTI 3; CSTI 4; LT ] [CSTI 1]

    [<TestMethod>]
    member this.ConstantFolding9 () =
        verifyOptimizationReturnsConstant [CSTI 3; CSTI 4; ADD; CSTI 5; CSTI 5; ADD; ADD]  

    [<TestMethod>]
    member this.ConstantFolding10 () =
        verifyOptimizationReturnsConstant [CSTI 3; CSTI 4; SUB; CSTI 5; CSTI 5; SUB; SUB] 

    [<TestMethod>]
    member this.ConstantFolding11 () =
        verifyOptimizationReturnsConstant [CSTI 3; CSTI 4; MUL; CSTI 5; CSTI 5; MUL; MUL] 

    [<TestMethod>]
    member this.ConstantFolding12 () =
        verifyOptimizationReturnsConstant [CSTI 3; CSTI 4; DIV; CSTI 5; CSTI 5; DIV; DIV]

    [<TestMethod>]
    member this.ConstantFolding13 () =
        verifyOptimizationReturnsConstant [CSTI 3; CSTI 4; MOD; CSTI 5; CSTI 5; MOD; MOD] 

    [<TestMethod>]
    member this.ConstantFolding14 () =
        verifyOptimizationReturnsConstant [CSTI 3; CSTI 4; EQ; CSTI 5; CSTI 5; EQ; EQ] 

    [<TestMethod>]
    member this.ConstantFolding15 () =
        verifyOptimizationReturnsConstant [CSTI 3; CSTI 4; LT; CSTI 5; CSTI 5; LT; LT] 

    [<TestMethod>]
    member this.ConstantFolding16 () =
        verifyOptimization [CSTI 0; GETBP; CSTI 1; ADD; LDI; ADD] [GETBP; CSTI 1; ADD; LDI] 

    [<TestMethod>]
    member this.ConstantFolding17 () =
        verifyOptimization [GETBP; CSTI 1; ADD; LDI; CSTI 0; ADD] [GETBP; CSTI 1; ADD; LDI] 

    [<TestMethod>]
    member this.ConstantFolding18 () =
        verifyOptimization [GETBP; CSTI 1; ADD; LDI; CSTI 0; SUB] [GETBP; CSTI 1; ADD; LDI]

    [<TestMethod>]
    member this.ConstantFolding19 () =
        verifyOptimization [CSTI 1; GETBP; CSTI 1; ADD; LDI; MUL] [GETBP; CSTI 1; ADD; LDI]

    [<TestMethod>]
    member this.ConstantFolding20 () =
        verifyOptimization [GETBP; CSTI 1; ADD; LDI; CSTI 1; MUL] [GETBP; CSTI 1; ADD; LDI]

    [<TestMethod>]
    member this.ConstantFolding21 () =
        verifyOptimization [GETBP; CSTI 1; ADD; LDI; CSTI 1; DIV] [GETBP; CSTI 1; ADD; LDI] 

    [<TestMethod>]
    member this.ConstantFolding22 () =
        verifyOptimization [CSTI 0; GETBP; CSTI 1; ADD; LDI; MOD] [GETBP; CSTI 1; ADD; LDI]

    [<TestMethod>]
    member this.ConstantFolding23 () =
        verifyOptimization [CSTI 0; GETBP; CSTI 0; ADD; LDI; ADD] [GETBP; LDI] 

    [<TestMethod>]
    member this.ConstantFolding24 () =
        verifyOptimization [GETBP; CSTI 0; ADD; LDI; CSTI 0; ADD] [GETBP; LDI] 

    [<TestMethod>]
    member this.ConstantFolding25 () =
        verifyOptimization [GETBP; CSTI 0; ADD; LDI; CSTI 0; SUB] [GETBP; LDI]

    [<TestMethod>]
    member this.ConstantFolding26 () =
        verifyOptimization [CSTI 1; GETBP; CSTI 0; ADD; LDI; MUL] [GETBP; LDI]

    [<TestMethod>]
    member this.ConstantFolding27 () =
        verifyOptimization [GETBP; CSTI 0; ADD; LDI; CSTI 1; MUL] [GETBP; LDI]

    [<TestMethod>]
    member this.ConstantFolding28 () =
        verifyOptimization [CSTI 0; GETBP; CSTI 0; ADD; LDI; MUL] []

    [<TestMethod>]
    member this.ConstantFolding29 () =
        verifyOptimization [GETBP; CSTI 0; ADD; LDI; CSTI 0; MUL] []

    [<TestMethod>]
    member this.ConstantFolding30 () =
        verifyOptimization [GETBP; CSTI 0; ADD; LDI; CSTI 1; DIV] [GETBP; LDI] 

    [<TestMethod>]
    member this.ConstantFolding31 () =
        verifyOptimization [GETBP; CSTI 0; ADD; LDI; CSTI 0; DIV] [] 

    [<TestMethod>]
    member this.ConstantFolding32 () =
        verifyOptimization [CSTI 0; GETBP; CSTI 0; ADD; LDI; MOD] [GETBP; LDI]