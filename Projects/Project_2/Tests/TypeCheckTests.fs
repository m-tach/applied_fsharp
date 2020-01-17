namespace Tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

open GuardedCommands.Util
open GuardedCommands.Frontend.TypeCheck
open GuardedCommands.Frontend.AST
open GuardedCommands.Backend.CodeGeneration

open ParserUtil
open CompilerUtil

[<TestClass>]
type TestTypeCheck () =

    [<TestMethod>]
    member this.TypeCheckA0 () =
        let ast = parseFromFile "programs/A0.gc"
        tcP ast |> ignore

    [<TestMethod>]
    member this.TypeCheckA1 () =
        let ast = parseFromFile "programs/A1.gc"
        tcP ast |> ignore

    [<TestMethod>]
    member this.TypeCheckA2 () =
        let ast = parseFromFile "programs/A2.gc"
        tcP ast |> ignore

    [<TestMethod>]
    member this.TypeCheckA3 () =
        let ast = parseFromFile "programs/A3.gc"
        tcP ast |> ignore

    [<TestMethod>]
    member this.TypeCheckA4 () =
        let ast = parseFromFile "programs/A4.gc"
        tcP ast |> ignore

    [<TestMethod>]
    member this.TypeCheckEx0 () =
        let ast = parseFromFile "programs/Ex0.gc"
        tcP ast |> ignore

    [<TestMethod>]
    member this.TypeCheckEx1 () =
        let ast = parseFromFile "programs/Ex1.gc"
        tcP ast |> ignore

    [<TestMethod>]
    member this.TypeCheckEx2 () =
        let ast = parseFromFile "programs/Ex2.gc"
        tcP ast |> ignore

    [<TestMethod>]
    member this.TypeCheckEx3 () =
        let ast = parseFromFile "programs/Ex3.gc"
        tcP ast |> ignore

    [<TestMethod>]
    member this.TypeCheckEx4 () =
        let ast = parseFromFile "programs/Ex4.gc"
        tcP ast |> ignore

    [<TestMethod>]
    member this.TypeCheckEx5 () =
        let ast = parseFromFile "programs/Ex5.gc"
        tcP ast |> ignore

    [<TestMethod>]
    member this.TypeCheckEx6 () =
        let ast = parseFromFile "programs/Ex6.gc"
        tcP ast |> ignore

    [<TestMethod>]
    member this.TypeCheckEx7 () =
        let ast = parseFromFile "programs/Ex7.gc"
        tcP ast |> ignore

    [<TestMethod>]
    member this.TypeCheckExa1 () =
        let ast = parseFromFile "programs/Exa1.gc"
        tcP ast |> ignore    

    [<TestMethod>]
    member this.TypeCheckFact () =
        let ast = parseFromFile "programs/fact.gc"
        tcP ast |> ignore

    [<TestMethod>]
    member this.TypeCheckFactRec () =
        let ast = parseFromFile "programs/factRec.gc"
        tcP ast |> ignore

    [<TestMethod>]
    member this.TypeCheckFactCBV () =
        let ast = parseFromFile "programs/factCBV.gc"
        tcP ast |> ignore

    [<TestMethod>]
    member this.TypeCheckFactImPTyp () =
        let ast = parseFromFile "programs/factImpPTyp.gc"
        tcP ast |> ignore

    [<TestMethod>]
    member this.TypeCheckGCDo () =
        let ast = parseFromFile "programs/GC_Do.gc"
        tcP ast |> ignore       

    [<TestMethod>]
    member this.TypeCheckNoReturnOutsideFunctions () =
        let program = @"
            begin
            res : int;
            res := 1;
            return res
            end"
        let ast = parseString program
        Assert.ThrowsException(fun _ -> tcP ast) |> ignore

    [<TestMethod>]
    member this.TypeCheckAllReturnsMustReturnFunctionType () =
        let program = @"
            begin
            res : int,
            function fact(n: int): int =
            {  
                return 1;
                return true
            } ;
            res := fact(4)
            end"
        let ast = parseString program
        Assert.ThrowsException(fun _ -> tcP ast) |> ignore    

    [<TestMethod>]
    member this.TypeCheckFunctionArgumentsAreOnlyVariables () =
        let program = @"
            begin
            res : int,
            function fact(n: int, function fact(n: int): int =
            {  
                return 1;
                return true
            }): int =
            {  
                return 1;
                return true
            } ;
            res := fact(4)
            end"
        let ast = parseString program
        Assert.ThrowsException(fun _ -> tcP ast) |> ignore

    [<TestMethod>]
    member this.TypeCheckAllReturnsWithinAFunctionReturnsAType () =
        let program = @"
            begin
            res : int,
            function fact(n: int): int =
            {  
                return 1;
                return
            } ;
            res := fact(4)
            end"
        let ast = parseString program
        Assert.ThrowsException(fun _ -> tcP ast) |> ignore  

    [<TestMethod>]
    member this.TypeCheckAllReturnsWithinAProcedureDoesNotReturnAType () =
        let program = @"
            begin
            res : int,
            procedure fact(n: int) =
            {  
                return 1;
                return
            } ;
            res := fact(4)
            end"
        let ast = parseString program
        Assert.ThrowsException(fun _ -> tcP ast) |> ignore

    [<TestMethod>]
    member this.TypeCheckProcedure1 () =
        let ast = parseFromFile "programs/basicProc1.gc"
        tcP ast |> ignore

    [<TestMethod>]
    member this.TypeCheckProcedure2 () =
        let ast = parseFromFile "programs/basicProc2.gc"
        tcP ast |> ignore

    [<TestMethod>]
    member this.TypeCheckProcedure3 () =
        let ast = parseFromFile "programs/basicProc3.gc"
        tcP ast |> ignore

    [<TestMethod>]
    member this.TypeCheckProcedure4 () =
        let ast = parseFromFile "programs/basicProc4.gc"
        tcP ast |> ignore    

    [<TestMethod>]
    member this.TypeCheckpar1 () =
        let ast = parseFromFile "programs/par1.gc"
        tcP ast |> ignore  

    [<TestMethod>]
    member this.TypeCheckpar2 () =
        let ast = parseFromFile "programs/par2.gc"
        tcP ast |> ignore      

    [<TestMethod>]
    member this.TypeCheckpartition () =
        let ast = parseFromFile "programs/partition.gc"
        tcP ast |> ignore     

    [<TestMethod>]
    member this.TypeCheckQuickSortV1 () =
        let ast = parseFromFile "programs/QuickSortV1.gc"
        tcP ast |> ignore  

    [<TestMethod>]
    member this.TypeCheckQuickSortV2 () =
        let ast = parseFromFile "programs/QuickSortV2.gc"
        tcP ast |> ignore         
        
    [<TestMethod>]
    member this.TypeCheckSkip () =
        let ast = parseFromFile "programs/skip.gc"
        tcP ast |> ignore   

    [<TestMethod>]
    member this.TypeCheckSwap () =
        let ast = parseFromFile "programs/swap.gc"
        tcP ast |> ignore

    [<TestMethod>]
    member this.TypeCheckPointers1 () =
        let ast = parseFromFile "programs/pointers3.gc"
        tcP ast |> ignore

    [<TestMethod>]
    member this.TypeCheckPointers2 () =
        let ast = parseFromFile "programs/pointers4.gc"
        tcP ast |> ignore

    [<TestMethod>]
    member this.TypeCheckPointers3 () =
        let ast = parseFromFile "programs/pointers5.gc"
        tcP ast |> ignore

    [<TestMethod>]
    member this.TypeCheckPointers4 () =
        let ast = parseFromFile "programs/pointers6.gc"
        tcP ast |> ignore                

    [<TestMethod>]
    member this.TypeCheckOrOperator () =
        let ast = parseFromFile "programs/OrOperator.gc"
        tcP ast |> ignore                