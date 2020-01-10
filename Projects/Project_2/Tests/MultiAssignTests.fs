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
type MultiAssignParse () =

    [<TestMethod>]
    member this.ParseMultiAssign1 () =
        (parseFromFile "programs/MultiAssign1.gc") |> ignore;

    [<TestMethod>]
    member this.ParseMultiAssign2 () =
        (parseFromFile "programs/MultiAssign2.gc") |> ignore;

    [<TestMethod>]
    member this.GlobalsMultiAssign1 () =
        // Parsing of file.gc
        let maTree = parseFromFile "programs/MultiAssign1.gc"
        let stack = goTrace maTree
        assert List.forall2 (fun(a, b) -> a == b) stack [1; 2; 3]

    [<TestMethod>]
    member this.GlobalsMultiAssign2 () =
        // Parsing of file.gc
        let maTree = parseFromFile "programs/MultiAssign2.gc"
        let stack = goTrace maTree
        assert List.forall2 (fun(a, b) -> a == b) stack [2; 1]
