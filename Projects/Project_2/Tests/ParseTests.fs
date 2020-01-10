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
        let ast = parseFromFile "programs/Ex0.gc"
        tcP ast
        CP ast

    [<TestMethod>]
    member this.ParseEx1 () =
        let ast = parseFromFile "programs/Ex1.gc"
        tcP ast
        CP ast

    [<TestMethod>]
    member this.ParseEx2 () =
        let ast = parseFromFile "programs/Ex2.gc"
        tcP ast
        CP ast

    [<TestMethod>]
    member this.ParseEx3 () =
        let ast = parseFromFile "programs/Ex3.gc"
        tcP ast
        CP ast

    [<TestMethod>]
    member this.ParseEx4 () =
        let ast = parseFromFile "programs/Ex4.gc"
        tcP ast
        CP ast

    [<TestMethod>]
    member this.ParseEx5 () =
        let ast = parseFromFile "programs/Ex5.gc"
        tcP ast
        CP ast

    [<TestMethod>]
    member this.ParseEx6 () =
        let ast = parseFromFile "programs/Ex6.gc"
        tcP ast
        CP ast

    //TODO: not supported - test fails
    [<TestMethod>]
    member this.ParseEx7 () =
        let ast = parseFromFile "programs/Ex7.gc"
        tcP ast
        CP ast

    [<TestMethod>]
    member this.ParseSkip () =
        let ast = parseFromFile "programs/Skip.gc"
        tcP ast
        CP ast

    [<TestMethod>]
    member this.ParseFact () =
        let ast = parseFromFile "programs/fact.gc"
        tcP ast
        CP ast

    [<TestMethod>]
    member this.ParseFactRec () =
        let ast = parseFromFile "programs/factRec.gc"
        tcP ast
        CP ast

    [<TestMethod>]
    member this.ParseFactCBV () =
        let ast = parseFromFile "programs/factCBV.gc"
        tcP ast
        CP ast
