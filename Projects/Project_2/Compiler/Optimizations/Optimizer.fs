namespace GuardedCommands.Backend

open System
open Machine
open GuardedCommands.Backend.ASTAnalyzer
open GuardedCommands.Backend.DeadCodeAnalyzer
open GuardedCommands.Backend.InstructionExpAnalyzer
open GuardedCommands.Backend.InstructionListAnalyzer
open GuardedCommands.Backend.BranchAnalyzer


module Optimizer =

    let rec public optimize instrs = 
        let fstOptiInstrs = optimizeInstrList instrs
        let sndOptiInstrs = optimizeBranching fstOptiInstrs 
        //printfn "%s" (sndOptiInstrs.Length.ToString())
        let noDeadCode = deadCodeOptimization sndOptiInstrs
        let exps = instrsToInstrsExp noDeadCode
        let optiExps = List.map optimizeInstrExp exps
        let optiInstrs = instrExpsToInstrs optiExps
        if instrs.Length <> optiInstrs.Length
        then optimize optiInstrs
        else optiInstrs                                   

