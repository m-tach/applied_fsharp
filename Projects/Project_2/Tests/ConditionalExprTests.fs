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
type TestConditionalExpressions () =

    [<TestMethod>]
    member this.TypeCheckCondExpr1 () =
        let ast = parseFromFile "programs/ConditionalExpr1.gc"
        tcP ast |> ignore

    [<TestMethod>]
    member this.TypeCheckCondExpr2 () =
        let ast = parseFromFile "programs/ConditionalExpr2.gc"
        tcP ast |> ignore
    
    [<TestMethod>]
    member this.CheckGlobals1 () =
        let maTree = parseFromFile "programs/ConditionalExpr1.gc"
        let stack = goTrace maTree
        assert List.forall2 (=) (Array.toList stack) [5; 4; 4]

    [<TestMethod>]
    member this.CheckGlobals2 () =
        let maTree = parseFromFile "programs/ConditionalExpr2.gc"
        let stack = goTrace maTree
        // i will become 15 from the do GC - so we get that i becomes 6 on the last line.
        assert List.forall2 (=) (Array.toList stack) [6]

    // Expects an error because the ternary operation has an int and a string as two possible outcomes.
    [<TestMethod>]
    member this.IllTyped () =
        let ast = parseFromFile "programs/ConditionalExprMismatchTypeFail.gc"
        Assert.ThrowsException(fun _ -> tcP ast) |> ignore
    