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
    let temp = "begin
    res : int;
    res := 2;
    print res
    end"


    //TODO: fails because functions are not implemented
    [<TestMethod>]
    member this.ParseA0 () =
        exec "programs/A0.gc" ;

    [<TestMethod>]
    member this.ParseA1 () =
        exec "programs/A1.gc" ;

    [<TestMethod>]
    member this.ParseA2 () =
        exec "programs/A2.gc" ;

    [<TestMethod>]
    member this.ParseA3 () =
        exec "programs/A3.gc" ;

    [<TestMethod>]
    member this.ParseA4 () =
        exec "programs/A4.gc" ;