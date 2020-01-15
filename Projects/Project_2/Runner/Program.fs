open GuardedCommands.Util
open GuardedCommands.Frontend.TypeCheck
open GuardedCommands.Frontend.AST
open GuardedCommands.Backend.CodeGeneration

open ParserUtil
open CompilerUtil
open Visualizer


[<EntryPoint>]
let main argv =
    let ex0Tree = parseFromFile "../programs/Char1.gc"
    let intrs = CP ex0Tree
    let intInstrs = Machine.code2ints intrs
    VirtualMachine.runTrace intInstrs |> ignore
    //Visualizer.Visualize ex0Tree
    0 // return an integer exit code
