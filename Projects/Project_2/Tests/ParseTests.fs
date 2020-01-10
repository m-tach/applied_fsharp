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
type TestParse () =

    [<TestMethod>]
    member this.ParseEx0 () =
        (parseFromFile "programs/Ex0.gc") |> ignore;

    [<TestMethod>]
    member this.ParseEx1 () =
        (parseFromFile "programs/Ex1.gc") |> ignore;

    [<TestMethod>]
    member this.ParseEx2 () =
        (parseFromFile "programs/Ex2.gc") |> ignore;

    [<TestMethod>]
    member this.ParseEx3 () =
        (parseFromFile "programs/Ex3.gc") |> ignore;

    [<TestMethod>]
    member this.ParseEx4 () =
        (parseFromFile "programs/Ex4.gc") |> ignore;

    [<TestMethod>]
    member this.ParseEx5 () =
        (parseFromFile "programs/Ex5.gc") |> ignore;

    [<TestMethod>]
    member this.ParseEx6 () =
        (parseFromFile "programs/Ex6.gc") |> ignore;

    //TODO: not supported - test fails
    [<TestMethod>]
    member this.ParseEx7 () =
        (parseFromFile "programs/Ex7.gc") |> ignore;

    [<TestMethod>]
    member this.ParseSkip () =
        (parseFromFile "programs/Skip.gc") |> ignore;

    [<TestMethod>]
    member this.ParseFact () =
        (parseFromFile "programs/fact.gc") |> ignore;

    [<TestMethod>]
    member this.ParseFactRec () =
        (parseFromFile "programs/factRec.gc") |> ignore;

    [<TestMethod>]
    member this.ParseFactCBV () =
        (parseFromFile "programs/factCBV.gc") |> ignore;
