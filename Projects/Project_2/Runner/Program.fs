open GuardedCommands.Util
open GuardedCommands.Frontend.TypeCheck
open GuardedCommands.Frontend.AST
open GuardedCommands.Backend.CodeGeneration
open GuardedCommands.Backend.Optimizer

open System.Diagnostics
open System

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
    let filename = "../programs/QuickSortV2.gc"
    //let filename = "../programs/factRec.gc"
    //let filename = "../programs/StringPrints.gc"

    let ex0Tree = parseFromFile filename
    let instrs = CP ex0Tree
    let optiInstrs = optimize instrs


    //run code
    let (execInstr, runtime) = VirtualMachine.runBenchmark(Machine.code2ints(instrs))
    let (opti_execInstr, opti_runtime) = VirtualMachine.runBenchmark(Machine.code2ints(optiInstrs))
    printfn "Running file %s" filename
    printfn ("\tNot optimized:")
    printfn "\t\tGenerated instructions: %s" (instrs.Length.ToString())
    printfn "\t\tExecuted instructions: %d" execInstr
    printfn "\t\tRuntime[ms]: %d" runtime

    printfn ("\tOptimized:")
    printfn "\t\tGenerated instructions: %s" (optiInstrs.Length.ToString())
    printfn "\t\tExecuted instructions: %d" opti_execInstr
    printfn "\t\tRuntime[ms]: %d" opti_runtime


    0 // return an integer exit code
