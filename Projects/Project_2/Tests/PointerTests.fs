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
type TestPointers () =

    [<TestMethod>]
    member this.TypeCheck1 () =
        let ast = parseFromFile "programs/Pointers1.gc"
        tcP ast |> ignore

    [<TestMethod>]
    member this.TypeCheckPar1 () =
        let ast = parseFromFile "programs/par1.gc"
        tcP ast |> ignore

    [<TestMethod>]
    member this.TypeCheckPar2 () =
        let ast = parseFromFile "programs/par2.gc"
        tcP ast |> ignore

    [<TestMethod>]
    member this.TypeCheckQuickSortV2 () =
        let ast = parseFromFile "programs/QuickSortV2.gc"
        tcP ast |> ignore

    [<TestMethod>]
    member this.CheckGlobals1 () =
        let maTree = parseFromFile "programs/Pointers1.gc"
        let stack = goTrace maTree
        assert List.forall2 (=) (Array.toList stack) [3; 5]

    [<TestMethod>]
    member this.CheckGlobalsPar1 () =
        let maTree = parseFromFile "programs/par1.gc"
        let stack = goTrace maTree
        assert List.forall2 (=) (Array.toList stack) [1]

    [<TestMethod>]
    member this.IllTyped2 () =
        let ast = parseFromFile "programs/Pointers2_Fail.gc"
        Assert.ThrowsException(fun _ -> tcP ast) |> ignore
    