namespace GuardedCommands.Backend

open System
open Machine

module BranchAnalyzer =
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
            | [] -> branches
        let rec optimizeShortJumps instrs jumpData =
            match instrs with
            | GOTO a   :: Label b :: rest when a = b && (Map.find a jumpData) = 1 -> optimizeShortJumps rest jumpData
            | GOTO a   :: Label b :: rest when a = b                              -> Label b :: optimizeShortJumps rest jumpData
            | a :: rest -> a :: optimizeShortJumps rest jumpData
            | [] -> []
        optimizeShortJumps allInstrs (findJumpData allInstrs Map.empty)

    let public optimizeBranching instrs =
        let optiInstrs = optimizeGoto instrs
        optimizeThroughBranches optiInstrs