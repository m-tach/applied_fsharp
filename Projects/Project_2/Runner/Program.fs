open GuardedCommands.Util
open GuardedCommands.Frontend.TypeCheck
open GuardedCommands.Frontend.AST
open GuardedCommands.Backend.CodeGeneration

open ParserUtil
open CompilerUtil
open Visualizer

let testFile str = 
    let ex0Tree = parseFromFile str
    let intrs = CP ex0Tree
    let intInstrs = Machine.code2ints intrs
    VirtualMachine.run intInstrs |> ignore

[<EntryPoint>]
let main argv =
    testFile "../programs/Preincrement1.gc" ;
    testFile "../programs/PreincDecLoop.gc" ;
    testFile "../programs/PredecrementLoop.gc" ;
    //Visualizer.Visualize ex0Tree
    0 // return an integer exit code
