open GuardedCommands.Util
open GuardedCommands.Frontend.TypeCheck
open GuardedCommands.Frontend.AST
open GuardedCommands.Backend.CodeGeneration

open ParserUtil
open CompilerUtil


[<EntryPoint>]
let main argv =
    // Parsing of Ex1.gc

    let ex6Tree = parseFromFile "../programs/GC_Do.gc"

    // -- is typechecked as follows:

    let _ = tcP ex6Tree

    // obtain symbolic code:
    let ex1Code = CP ex6Tree 

    // -- is executed with trace as follows:
    let stack = goTrace ex6Tree

    // -- is executed as follows (no trace):
    let sameStack = go ex6Tree

    // "All in one" parse from file, type check, compile and run 

    let _ = exec "../programs/Ex6.gc"

    // Test of programs covered by the fifth task using optimized compilation (Section 8.2):
    //List.iter execOpt ["../programs/Ex1.gc"; "../programs/Ex2.gc"]

    // All programs relating to the basic version can be parsed:
    //let pts = List.map parseFromFile ["../programs/Ex1.gc"; "../programs/Ex2.gc";"../programs/Ex3.gc"; "../programs/Ex4.gc"; "../programs/Ex5.gc"; "../programs/Ex6.gc"; "../programs/Skip.gc"]

    // The parse tree for Ex3.gc
    //List.item 2 pts 

    (*
    // Test of programs covered by the first task (Section 3.7):
    List.iter exec ["programs/Ex1.gc"; "programs/Ex2.gc";"programs/Ex3.gc"; "programs/Ex4.gc"; "programs/Ex5.gc"; "programs/Ex6.gc"; "programs/Skip.gc"]

    // Test of programs covered by the second task (Section 4.3):
    List.iter exec ["programs/Ex7.gc"; "programs/fact.gc"; "programs/factRec.gc"; "programs/factCBV.gc"]

    // Test of programs covered by the fourth task (Section 5.4):
    List.iter exec ["programs/A0.gc"; "programs/A1.gc"; "programs/A2.gc"; "programs/A3.gc"]

    // Test of programs covered by the fifth task (Section 6.1):
    List.iter exec ["programs/A4.gc"; "programs/Swap.gc"; "programs/QuickSortV1.gc"]

    // Test of programs covered by the fifth task (Section 7.4):
    List.iter exec ["programs/par1.gc"; "programs/factImpPTyp.gc"; "programs/QuickSortV2.gc"; "programs/par2.gc"]

    // Test of programs covered by the fifth task using optimized compilation (Section 8.2):
    List.iter execOpt ["programs/par1.gc"; "programs/factImpPTyp.gc"; "programs/QuickSortV2.gc"; "programs/par2.gc"];

    *)
    0 // return an integer exit code
