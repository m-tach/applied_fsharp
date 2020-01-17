namespace GuardedCommands.Backend

open System
open Machine
open GuardedCommands.Backend.ASTAnalyzer
open GuardedCommands.Backend.DeadCodeAnalyzer
open GuardedCommands.Backend.InstructionExpAnalyzer
open GuardedCommands.Backend.InstructionListAnalyzer


module Optimizer =

    let instrsToString instrs = 
       (String.concat "\n" (List.map (fun x ->match x with
                                              | CSTI i -> String.Format("CSTI {0}", i)
                                              | ADD -> "ADD"
                                              | SUB -> "SUB"
                                              | MUL -> "MUL"
                                              | DIV -> "DIV"
                                              | MOD -> "MOD"
                                              | EQ -> "EQ"
                                              | LT -> "LT"
                                              | NOT -> "NOT"
                                              | DUP -> "DUP"
                                              | SWAP -> "SWAP"
                                              | LDI -> "LDI"
                                              | STI -> "STI"
                                              | GETBP -> "GETBP"
                                              | GETSP -> "GETSP"
                                              | INCSP m -> String.Format("INCSP {0}", m)
                                              | GOTO a -> String.Format("GOTO {0}", a)
                                              | IFZERO a -> String.Format("IFZERO {0}", a)
                                              | IFNZRO a -> String.Format("IFNZRO {0}", a)
                                              | CALL(m, a) -> String.Format("CALL {0} {1}", m, a)
                                              | TCALL(m, n, a) -> String.Format("TCALL {0} {1} {2}", m, n, a)
                                              | RET m -> String.Format("RET {0}", m)
                                              | PRINTI -> "PRINTI"
                                              | PRINTC -> "PRINTC"
                                              | LDARGS -> "LDARGS"
                                              | STOP -> "STOP"
                                              | Label a -> String.Format("Label {0}", a)) instrs))    

    let rec private optimizeGoto allInstrs =
        let rec replaceGotoTarget instrs fromLab toLab =
            match instrs with
            | GOTO a         :: rest when a = fromLab -> GOTO toLab         :: replaceGotoTarget rest fromLab toLab
            | IFZERO a       :: rest when a = fromLab -> IFZERO toLab       :: replaceGotoTarget rest fromLab toLab
            | IFNZRO a       :: rest when a = fromLab -> IFNZRO toLab       :: replaceGotoTarget rest fromLab toLab
            | CALL(m, a)     :: rest when a = fromLab -> CALL(m, toLab)     :: replaceGotoTarget rest fromLab toLab
            | TCALL(m, n, a) :: rest when a = fromLab -> TCALL(m, n, toLab) :: replaceGotoTarget rest fromLab toLab
            | a :: rest -> a :: replaceGotoTarget rest fromLab toLab
            | [] -> []
        let rec optimizeGotointernal instrs = 
            match instrs with
            | Label a :: GOTO b  :: rest -> (GOTO b  :: rest, a, b, true)
            | Label a :: Label b :: rest -> (Label b :: rest, a, b, true)
            | a :: rest -> let (b, c, d, e) = optimizeGotointernal rest
                           (a::b, c, d, e)
            | [] -> ([], "", "", false)
        let rec loopUntilNoChange instrs = 
            match optimizeGotointernal instrs with
            | (chr, fromLab, toLab, true) -> loopUntilNoChange (replaceGotoTarget chr fromLab toLab)
            | (chr, _, _, _) -> chr
        loopUntilNoChange allInstrs

    let rec private optimizeThroughBranches allInstrs =
        let rec findJumpData instrs branches =
            match instrs with
            | GOTO a :: rest -> let branches2 = match Map.containsKey a branches with
                                                | true -> Map.add a ((Map.find a branches) + 1) branches
                                                |_     -> Map.add a 1 branches
                                findJumpData rest branches2  
            | IFZERO a :: rest -> let branches2 = match Map.containsKey a branches with
                                                  | true -> Map.add a ((Map.find a branches) + 1) branches
                                                  |_     -> Map.add a 1 branches
                                  findJumpData rest branches2
            | IFNZRO a :: rest -> let branches2 = match Map.containsKey a branches with
                                                  | true -> Map.add a ((Map.find a branches) + 1) branches
                                                  |_     -> Map.add a 1 branches
                                  findJumpData rest branches2
            | CALL(_, a) :: rest -> let branches2 = match Map.containsKey a branches with
                                                    | true -> Map.add a ((Map.find a branches) + 1) branches
                                                    |_     -> Map.add a 1 branches
                                    findJumpData rest branches2
            | TCALL(_, _, a) :: rest -> let branches2 = match Map.containsKey a branches with
                                                        | true -> Map.add a ((Map.find a branches) + 1) branches
                                                        |_     -> Map.add a 1 branches
                                        findJumpData rest branches2  
            | _ :: rest -> findJumpData rest branches 
            | [] -> printfn "%A" (Map.toList branches)
                    branches
        let rec findToBranch instrs branch =
            match instrs with
            | GOTO a         :: _ when a = branch -> [GOTO a]
            | IFZERO a       :: _ when a = branch -> [IFZERO a]
            | IFNZRO a       :: _ when a = branch -> [IFNZRO a]
            | CALL(m, a)     :: _ when a = branch -> [CALL(m, a)]
            | TCALL(m, n, a) :: _ when a = branch -> [TCALL(m, n, a)]
            | a :: rest -> a :: findToBranch rest branch 
            | [] -> failwith "failed to find the branch"
        let rec findAfterLabel instrs lab =
            match instrs with
            | Label a :: rest when a = lab -> Label a :: rest
            | _ :: rest -> findAfterLabel rest lab
            | [] -> failwith (String.Format("failed to find the label {0}", lab))
        let rec optimizeJump beforeJump afterJump =
            match (List. rev beforeJump, afterJump) with
            | (GOTO _ :: rest1, Label _ :: IFZERO c :: rest2) -> ((List.rev (IFZERO c :: rest1)) @ (IFZERO c :: rest2), true)
            | (GOTO _ :: rest1, Label _ :: IFNZRO c :: rest2) -> ((List.rev (IFNZRO c :: rest1)) @ (IFNZRO c :: rest2), true)
            | (GOTO _ :: CSTI 0 :: rest1, Label _ :: IFZERO c :: rest2) ->             ((List.rev (GOTO c :: rest1)) @ (IFZERO c :: rest2), true)
            | (GOTO _ :: CSTI a :: rest1, Label _ :: IFNZRO c :: rest2) when a <> 0 -> ((List.rev (GOTO c :: rest1)) @ (IFNZRO c :: rest2), true)
            | _ -> ([], false)
        let rec optimizeJumps instrs jumpData =
            match jumpData with
            | (lab, 1) :: rest -> printfn "\n\n\n\n\n%s" (instrsToString instrs)
                                  let beforeJump = findToBranch instrs lab
                                  let afterJump  = findAfterLabel instrs lab
                                  match optimizeJump beforeJump afterJump with
                                  | (opti, true) -> optimizeJumps opti rest
                                  | (_, false) -> optimizeJumps instrs rest
            | (_, _) :: rest -> optimizeJumps instrs rest
            | [] -> instrs
        optimizeJumps allInstrs (Map.toList (findJumpData allInstrs Map.empty))

    let public optimizeBranching instrs =
        let optiInstrs = optimizeGoto instrs
        optimizeThroughBranches optiInstrs

    let rec public optimize instrs = 
        let fstOptiInstrs = optimizeInstrList instrs
        let sndOptiInstrs = optimizeBranching fstOptiInstrs 
        printfn "%s" (sndOptiInstrs.Length.ToString())
        let noDeadCode = deadInstrElimination sndOptiInstrs
        let exps = instrsToInstrsExp noDeadCode
        let optiExps = List.map optimizeInstrExp exps
        let optiInstrs = instrExpsToInstrs optiExps
        if instrs.Length <> optiInstrs.Length
        then optimize optiInstrs
        else optiInstrs                                   

