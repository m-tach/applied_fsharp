open GuardedCommands.Util
open GuardedCommands.Frontend.TypeCheck
open GuardedCommands.Frontend.AST
open GuardedCommands.Backend.CodeGeneration

open ParserUtil
open CompilerUtil

let testFile fileName =
    // Parsing of file.gc
    let ex1Tree = parseFromFile fileName
    // -- is typechecked as follows:
    let _ = tcP ex1Tree
    // obtain symbolic code:
    let ex1Code = CP ex1Tree 
    // -- is executed with trace as follows:
    let stack = goTrace ex1Tree
    // -- is executed as follows (no trace):
    let sameStack = go ex1Tree
    ()

[<EntryPoint>]
let main argv =
    testFile "../programs/Pointers1.gc"

    0