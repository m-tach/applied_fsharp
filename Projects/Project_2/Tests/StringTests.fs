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
type StringTests() =

    [<TestMethod>]
    member this.ParseStringAssignments () =
        exec "programs/StringAssignments.gc" ;

    [<TestMethod>]
    member this.ParseStringPrints () =
        exec "programs/StringPrints.gc" ;

    [<TestMethod>]
    member this.ParseStringIndexing () =
        exec "programs/StringIndexing.gc" ;

    [<TestMethod>]
    member this.CharEquals () =
        exec "programs/CharEquals.gc" ;

    //expect an error because chars should not contain strings
    [<TestMethod>]
    member this.CharNotStringFail () =
        Assert.ThrowsException (fun t -> exec "programs/CharNotStringFail.gc") ;