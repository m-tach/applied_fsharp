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
    member this.TypeCheckSkip () =
        let ast = parseFromFile "programs/Skip.gc"
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