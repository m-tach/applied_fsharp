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

    // Can we parse the MultiAssign file x,y,z := 1,2,3;
    [<TestMethod>]
    member this.ParseMultiAssign1 () =
        (parseFromFile "programs/MultiAssign1.gc") |> ignore;

    // Can we parse the MultiAssign file b,x := true,2;
    [<TestMethod>]
    member this.ParseMultiAssign2 () =
        (parseFromFile "programs/MultiAssign2.gc") |> ignore;

    // After doing x,y,z := 1,2,3, check that globals are 1,2,3 (x,y,z)
    [<TestMethod>]
    member this.GlobalsMultiAssign1 () =
        // Parsing of file.gc
        let maTree = parseFromFile "programs/MultiAssign1.gc"
        let stack = goTrace maTree
        Assert.IsTrue (List.forall2 (=) (Array.toList stack) [1; 2; 3])

    // After doing b,x := true,2, check that globals are 2,1 (x,b)
    [<TestMethod>]
    member this.GlobalsMultiAssign2 () =
        // Parsing of file.gc
        let maTree = parseFromFile "programs/MultiAssign2.gc"
        let stack = goTrace maTree
        Assert.IsTrue (List.forall2 (=) (Array.toList stack) [2; 1])
    
    // Check against too many values. Assignment left/right side must have same amount of variables & values.
    [<TestMethod>]
    member this.MultiAssignTooManyValues () =
        Assert.ThrowsException(fun () -> // Parsing of file.gc
                                         let maTree = parseFromFile "programs/MultiAssign3_Fail.gc"
                                         tcP maTree
        ) |> ignore
    
    // Check against invalid syntax. This is caught in the parser.
    [<TestMethod>]
    member this.MultiAssignInvalidSyntaxTokens () =
        Assert.ThrowsException(fun () -> // Parsing of file.gc
                                         let maTree = parseFromFile "programs/MultiAssign4_Fail.gc"
                                         tcP maTree
        ) |> ignore
    
    // Check against invalid types. This is caught in the type checker.
    [<TestMethod>]
    member this.MultiAssignWrongTypes () =
        Assert.ThrowsException(fun () -> // Parsing of file.gc
                                         let maTree = parseFromFile "programs/MultiAssign5_Fail.gc"
                                         tcP maTree
        ) |> ignore