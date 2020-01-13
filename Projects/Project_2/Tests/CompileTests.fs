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
type TestCompile () =

    [<TestMethod>]
    member this.CompileEx0 () =
        let ast = parseFromFile "programs/Ex0.gc"
        CP ast |> ignore

    [<TestMethod>]
    member this.CompileEx1 () =
        let ast = parseFromFile "programs/Ex1.gc"
        CP ast |> ignore

    [<TestMethod>]
    member this.CompileEx2 () =
        let ast = parseFromFile "programs/Ex2.gc"
        CP ast |> ignore

    [<TestMethod>]
    member this.CompileEx3 () =
        let ast = parseFromFile "programs/Ex3.gc"
        CP ast |> ignore

    [<TestMethod>]
    member this.CompileEx4 () =
        let ast = parseFromFile "programs/Ex4.gc"
        CP ast |> ignore

    [<TestMethod>]
    member this.CompileEx5 () =
        let ast = parseFromFile "programs/Ex5.gc"
        CP ast |> ignore

    [<TestMethod>]
    member this.CompileEx6 () =
        let ast = parseFromFile "programs/Ex6.gc"
        CP ast |> ignore

    [<TestMethod>]
    member this.CompileEx7 () =
        let ast = parseFromFile "programs/Ex7.gc"
        CP ast |> ignore

    [<TestMethod>]
    member this.CompileSkip () =
        let ast = parseFromFile "programs/Skip.gc"
        CP ast |> ignore

    [<TestMethod>]
    member this.CompileFact () =
        let ast = parseFromFile "programs/fact.gc"
        CP ast |> ignore

    [<TestMethod>]
    member this.CompileFactRec () =
        let ast = parseFromFile "programs/factRec.gc"
        CP ast |> ignore

    [<TestMethod>]
    member this.CompileFactCBV () =
        let ast = parseFromFile "programs/factCBV.gc"
        CP ast |> ignore

    [<TestMethod>]
    member this.CompileProcedure1 () =
        let ast = parseFromFile "programs/basicProc1.gc"
        CP ast |> ignore

    [<TestMethod>]
    member this.CompileProcedure2 () =
        let ast = parseFromFile "programs/basicProc2.gc"
        CP ast |> ignore    
