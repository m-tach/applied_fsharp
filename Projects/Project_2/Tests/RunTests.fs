namespace Tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

open GuardedCommands.Util
open GuardedCommands.Frontend.TypeCheck
open GuardedCommands.Frontend.AST
open GuardedCommands.Backend.CodeGeneration
open GuardedCommands.Backend.Optimizer

open ParserUtil
open CompilerUtil

[<TestClass>]
type TestRun () =

    let runInstrs instrs =
        Array.toList(VirtualMachine.execcode(List.toArray (Machine.code2ints instrs), (Array.zeroCreate 10000), (Array.zeroCreate 0)) false)

    let runAST ast =
        let instrs = CP ast
        let optiInstrs = optimize instrs
        Assert.AreEqual(runInstrs instrs, runInstrs optiInstrs)

    [<TestMethod>]
    member this.RunA0 () =
        let ast = parseFromFile "programs/A0.gc"
        runAST ast

    [<TestMethod>]
    member this.RunA1 () =
        let ast = parseFromFile "programs/A1.gc"
        runAST ast

    [<TestMethod>]
    member this.RunA2 () =
        let ast = parseFromFile "programs/A2.gc"
        runAST ast

    [<TestMethod>]
    member this.RunA3 () =
        let ast = parseFromFile "programs/A3.gc"
        runAST ast

    [<TestMethod>]
    member this.RunA4 () =
        let ast = parseFromFile "programs/A4.gc"
        runAST ast

    [<TestMethod>]
    member this.RunEx0 () =
        let ast = parseFromFile "programs/Ex0.gc"
        runAST ast

    [<TestMethod>]
    member this.RunEx1 () =
        let ast = parseFromFile "programs/Ex1.gc"
        runAST ast

    [<TestMethod>]
    member this.RunEx2 () =
        let ast = parseFromFile "programs/Ex2.gc"
        runAST ast

    [<TestMethod>]
    member this.RunEx3 () =
        let ast = parseFromFile "programs/Ex3.gc"
        runAST ast

    [<TestMethod>]
    member this.RunEx4 () =
        let ast = parseFromFile "programs/Ex4.gc"
        runAST ast

    [<TestMethod>]
    member this.RunEx5 () =
        let ast = parseFromFile "programs/Ex5.gc"
        runAST ast

    [<TestMethod>]
    member this.RunEx6 () =
        let ast = parseFromFile "programs/Ex6.gc"
        runAST ast

    [<TestMethod>]
    member this.RunEx7 () =
        let ast = parseFromFile "programs/Ex7.gc"
        runAST ast

    [<TestMethod>]
    member this.RunExa1 () =
        let ast = parseFromFile "programs/Exa1.gc"
        runAST ast

    [<TestMethod>]
    member this.RunFact () =
        let ast = parseFromFile "programs/fact.gc"
        runAST ast

    [<TestMethod>]
    member this.RunFactRec () =
        let ast = parseFromFile "programs/factRec.gc"
        runAST ast

    [<TestMethod>]
    member this.RunFactCBV () =
        let ast = parseFromFile "programs/factCBV.gc"
        runAST ast

    [<TestMethod>]
    member this.RunFactImPTyp () =
        let ast = parseFromFile "programs/factImpPTyp.gc"
        runAST ast

    [<TestMethod>]
    member this.RunFactGCDo () =
        let ast = parseFromFile "programs/GC_Do.gc"
        runAST ast      

    [<TestMethod>]
    member this.RunProcedure1 () =
        let ast = parseFromFile "programs/basicProc1.gc"
        runAST ast

    [<TestMethod>]
    member this.RunProcedure2 () =
        let ast = parseFromFile "programs/basicProc2.gc"
        runAST ast

    [<TestMethod>]
    member this.RunProcedure3 () =
        let ast = parseFromFile "programs/basicProc3.gc"
        runAST ast

    [<TestMethod>]
    member this.RunProcedure4 () =
        let ast = parseFromFile "programs/basicProc4.gc"
        runAST ast    

    [<TestMethod>]
    member this.Runpar1 () =
        let ast = parseFromFile "programs/par1.gc"
        runAST ast

    [<TestMethod>]
    member this.Runpar2 () =
        let ast = parseFromFile "programs/par2.gc"
        runAST ast    

    [<TestMethod>]
    member this.Runpartition () =
        let ast = parseFromFile "programs/partition.gc"
        runAST ast     

    [<TestMethod>]
    member this.RunQuickSortV1 () =
        let ast = parseFromFile "programs/QuickSortV1.gc"
        runAST ast

    [<TestMethod>]
    member this.RunQuickSortV2 () =
        let ast = parseFromFile "programs/QuickSortV2.gc"
        runAST ast 

    [<TestMethod>]
    member this.RunSkip () =
        let ast = parseFromFile "programs/skip.gc"
        runAST ast 

    [<TestMethod>]
    member this.RunSwap () =
        let ast = parseFromFile "programs/swap.gc"
        runAST ast       

    [<TestMethod>]
    member this.RunPointers1 () =
        let ast = parseFromFile "programs/pointers3.gc"
        runAST ast

    [<TestMethod>]
    member this.RunPointers2 () =
        let ast = parseFromFile "programs/pointers4.gc"
        runAST ast

    [<TestMethod>]
    member this.RunPointers3 () =
        let ast = parseFromFile "programs/pointers5.gc"
        runAST ast

    [<TestMethod>]
    member this.RunPointers4 () =
        let ast = parseFromFile "programs/pointers6.gc"
        runAST ast             

    [<TestMethod>]
    member this.RunOrOperator () =
        let ast = parseFromFile "programs/OrOperator.gc"
        let intrs = CP ast
        let intInstrs = Machine.code2ints intrs
        VirtualMachine.run intInstrs |> ignore             

    [<TestMethod>]
    member this.RunDivideOperator () =
        let ast = parseString "begin
                                res : int, x : int;
                                res := 6;
                                x := res / 2
                               end"
        let intrs = CP ast
        let resultStack = runInstrs intrs
        let expectedStack = [6; 3]
        Assert.AreEqual(expectedStack, resultStack)
