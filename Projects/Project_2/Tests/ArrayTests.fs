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
type ArrayTests() =
    [<TestMethod>]
    member this.ParseA0 () =
        tcP (parseFromFile "programs/A0.gc") |> ignore;