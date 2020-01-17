open GuardedCommands.Util
open GuardedCommands.Frontend.TypeCheck
open GuardedCommands.Frontend.AST
open GuardedCommands.Backend.CodeGeneration
open GuardedCommands.Backend.Optimizer

open System.Diagnostics

open ParserUtil
open CompilerUtil
open Visualizer

let runInstrs instrs =
    Array.toList(VirtualMachine.execcode(List.toArray (Machine.code2ints instrs), (Array.zeroCreate 10000), (Array.zeroCreate 0)) false)

let runAST ast =
    let instrs = CP ast
    let optiInstrs = optimize instrs
    printfn "%A" (runInstrs instrs)
    printfn "%A" (runInstrs optiInstrs)
    //VirtualMachine.runTrace (Machine.code2ints optiInstrs)

[<EntryPoint>]
let main argv =
    let ex0Tree = parseFromFile "../programs/QuickSortV2.gc"
    let instrs = CP ex0Tree
    let optiInstrs = optimize instrs
    
    printfn "not optimized\n%s" (instrsToString instrs)
    printfn "\n\noptimized\n%s" (instrsToString optiInstrs)
    printfn ""
    printfn "no opti:   %s" (instrs.Length.ToString())
    printfn "with opti: %s" (optiInstrs.Length.ToString())

    runAST ex0Tree
    
    //let intInstrs = Machine.code2ints optiInstrs
    //VirtualMachine.runTrace intInstrs |> ignore
    //Visualizer.Visualize ex0Tree
    0 // return an integer exit code
