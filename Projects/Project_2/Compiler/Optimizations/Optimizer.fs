namespace GuardedCommands.Backend

open System
open Machine
open GuardedCommands.Backend.ASTAnalyzer
open GuardedCommands.Backend.DeadCodeAnalyzer
open GuardedCommands.Backend.InstructionExpAnalyzer
open GuardedCommands.Backend.InstructionListAnalyzer

module Optimizer =

    let rec private optimizeGoto allInstrs =
        let rec replaceGotoTarget instrs fromLab toLab =
            match instrs with
            | GOTO a   :: rest when a = fromLab -> GOTO toLab   :: replaceGotoTarget rest fromLab toLab
            | IFZERO a :: rest when a = fromLab -> IFZERO toLab :: replaceGotoTarget rest fromLab toLab
            | IFNZRO a :: rest when a = fromLab -> IFNZRO toLab :: replaceGotoTarget rest fromLab toLab
            | CALL(m, a) :: rest when a = fromLab -> CALL(m, toLab) :: replaceGotoTarget rest fromLab toLab
            | TCALL(m, n, a) :: rest when a = fromLab -> TCALL(m, n, toLab) :: replaceGotoTarget rest fromLab toLab
            | a :: rest -> a :: replaceGotoTarget rest fromLab toLab
            | [] -> []
        let rec optimizeGotointernal instrs = 
            match instrs with
            | Label a :: GOTO b         :: rest -> (GOTO b         :: rest, a, b, true)
            | a :: rest -> let (b, c, d, e) = optimizeGotointernal rest
                           (a::b, c, d, e)
            | [] -> ([], "", "", false)
        let rec loopUntilNoChange instrs = 
            let (chr, fromLab, toLab, foundPattern) = optimizeGotointernal instrs
            let optiInstrs = if foundPattern 
                             then  replaceGotoTarget chr fromLab toLab
                             else chr
            if optiInstrs.Length <> instrs.Length
            then loopUntilNoChange optiInstrs
            else optiInstrs
        loopUntilNoChange allInstrs

    let rec public optimize instrs = 
        let firstOptiInstrs = optimizeInstrList instrs
        let noDeadCode = deadInstrElimination firstOptiInstrs
        let exps = instrsToInstrsExp noDeadCode
        let optiExps = List.map optimizeInstrExp exps
        let optiInstrs = instrExpsToInstrs optiExps
        if instrs.Length <> optiInstrs.Length
        then optimize optiInstrs
        else optimizeGoto optiInstrs                                   

