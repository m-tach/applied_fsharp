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

    //TODO: not supported - test fails
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
