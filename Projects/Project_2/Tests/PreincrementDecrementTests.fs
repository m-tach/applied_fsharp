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
type PreincrementDecrementTests() =

    // Perform ++i twice, so the value becomes 2.
    [<TestMethod>]
    member this.TestPreincrement () =
        let p = parseFromFile "programs/Preincrement1.gc"
        Assert.IsTrue (List.forall2 (=) (List.ofArray (goTrace p)) [2]) ;

    // Define i = 5, j = 0. Decrements j (thus -1), and loops ++i until i becomes 10.
    [<TestMethod>]
    member this.TestPreincPredecLoop () =
        let p = parseFromFile "programs/PreincDecLoop.gc" ;
        Assert.IsTrue (List.forall2 (=) (List.ofArray (goTrace p)) [10; -1]) ;

    //expect an error because only integer values supports preincrement and predecrement
    [<TestMethod>]
    member this.TestPreincBoolFail () =
        Assert.ThrowsException (fun t -> exec "programs/PreincrementOnBoolFail.gc") |> ignore

    // Defines i = 10, decrements i so long i > 0.
    [<TestMethod>]
    member this.TestPredecrementLoop () =
        let p = parseFromFile "programs/PredecrementLoop.gc" ;
        Assert.IsTrue (List.forall2 (=) (List.ofArray (goTrace p)) [0]) ;