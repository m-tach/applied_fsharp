open GuardedCommands.Util
open GuardedCommands.Frontend.TypeCheck
open GuardedCommands.Frontend.AST
open GuardedCommands.Backend.CodeGeneration
open GuardedCommands.Backend.Optimizer

open ParserUtil
open CompilerUtil
open Visualizer


[<EntryPoint>]
let main argv =
    let ex0Tree = parseFromFile "../programs/fact.gc"
    let instrs = CP ex0Tree
    let optiInstrs = optimize instrs
    
    printfn "not optimized\n%s" (instrsToString instrs)
    printfn "\n\noptimized\n%s" (instrsToString optiInstrs)
    printfn ""
    printfn "no opti:   %s" (instrs.Length.ToString())
    printfn "with opti: %s" (optiInstrs.Length.ToString())
    
    //let intInstrs = Machine.code2ints optiInstrs
    //VirtualMachine.runTrace intInstrs |> ignore
    //Visualizer.Visualize ex0Tree
    0 // return an integer exit code
