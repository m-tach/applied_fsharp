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
        CP (parseFromFile "programs/Ex0.gc") |> ignore;

    [<TestMethod>]
    member this.ParseEx1 () =
        CP (parseFromFile "programs/Ex1.gc") |> ignore;

    [<TestMethod>]
    member this.ParseEx2 () =
        CP (parseFromFile "programs/Ex2.gc") |> ignore;

    [<TestMethod>]
    member this.ParseEx3 () =
        CP (parseFromFile "programs/Ex3.gc") |> ignore;

    [<TestMethod>]
    member this.ParseEx4 () =
        CP (parseFromFile "programs/Ex4.gc") |> ignore;

    [<TestMethod>]
    member this.ParseEx5 () =
        CP (parseFromFile "programs/Ex5.gc") |> ignore;

    [<TestMethod>]
    member this.ParseEx6 () =
        CP (parseFromFile "programs/Ex6.gc") |> ignore;

    //TODO: not supported - test fails
    [<TestMethod>]
    member this.ParseEx7 () =
        CP (parseFromFile "programs/Ex7.gc") |> ignore;

    [<TestMethod>]
    member this.ParseSkip () =
        CP (parseFromFile "programs/Skip.gc") |> ignore;

    [<TestMethod>]
    member this.ParseFact () =
        CP (parseFromFile "programs/fact.gc") |> ignore;

    [<TestMethod>]
    member this.ParseFactRec () =
        CP (parseFromFile "programs/factRec.gc") |> ignore;

    [<TestMethod>]
    member this.ParseFactCBV () =
        CP (parseFromFile "programs/factCBV.gc") |> ignore;
