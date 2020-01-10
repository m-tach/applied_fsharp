namespace Tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

open GuardedCommands.Util
open GuardedCommands.Frontend.TypeCheck
open GuardedCommands.Frontend.AST
open GuardedCommands.Backend.CodeGeneration

open ParserUtil
open CompilerUtil

module Assert = 
  let Throws<'a> f =
    let mutable wasThrown = false
    try
      f()
    with
    | ex -> Assert.AreEqual(ex.GetType(), typedefof<'a>, (sprintf "Actual Exception: %A" ex)); wasThrown <- true

    Assert.IsTrue(wasThrown, "No exception thrown")

    
[<TestClass>]
type TestParse () =
    let missingDeclaration = "begin 
    res := 4;
    print res + 2 
    end"

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
    // Testing that an exception is thrown when a variable is not declared before its used
    [<TestMethod>]
    member this.VariableDeclarationException () =
        Assert.Throws<Exception>( fun() -> tcP (parseString missingDeclaration) )
        