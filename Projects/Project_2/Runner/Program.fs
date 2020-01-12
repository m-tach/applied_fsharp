open GuardedCommands.Util
open GuardedCommands.Frontend.TypeCheck
open GuardedCommands.Frontend.AST
open GuardedCommands.Backend.CodeGeneration

open ParserUtil
open CompilerUtil
open Visualizer

open Machine
open VirtualMachine


[<EntryPoint>]
let main argv =
    let ex0Tree = parseFromFile "../programs/fact.gc"
    let intrs = CP ex0Tree
    VirtualMachine.run (Machine.code2ints intrs) |> ignore
    //Visualizer.Visualize ex0Tree
    0 // return an integer exit code
