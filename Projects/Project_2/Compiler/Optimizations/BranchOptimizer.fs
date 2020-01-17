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
        let rec replacePattern instrs fromPat toPat =
            match (instrs, fromPat, toPat) with
            | (_, [], _) -> instrs
            | (_, _, []) -> failwith "pattern to replace and pattern to eplace with should have the same length"
            | ([], _, _) -> failwith "couldn't replace the pattern"
            | (a :: instrsRest, b :: fromRest, c :: toRest) when a = b -> c :: replacePattern instrsRest fromRest toRest
            | (a :: instrsRest, _, _) -> a :: replacePattern instrsRest fromPat toPat        
        let rec optimizeJump instrs beforeJump afterJump =
            match (List. rev beforeJump, afterJump) with
            | (GOTO a :: rest1, Label b :: IFZERO c :: rest2) -> let instrs2 = (replacePattern instrs [GOTO a] [IFZERO c])
                                                                 (replacePattern instrs2 [Label b] [INCSP 0], true)
                                                                 //((List.rev (IFZERO c :: rest1)) @ (IFZERO c :: rest2), true)
            | (GOTO a :: rest1, Label b :: IFNZRO c :: rest2) -> let instrs2 = (replacePattern instrs [GOTO a] [IFNZRO c])
                                                                 (replacePattern instrs2 [Label b] [INCSP 0], true)
                                                                 //((List.rev (IFNZRO c :: rest1)) @ (IFNZRO c :: rest2), true)
            | (GOTO a :: CSTI 0 :: rest1, Label b :: IFZERO c :: rest2) -> let instrs2 = (replacePattern instrs [GOTO a] [GOTO c])
                                                                           (replacePattern instrs2 [Label b] [INCSP 0], true)
                                                                           //((List.rev (GOTO c :: rest1)) @ (IFZERO c :: rest2), true)
            | (GOTO x :: CSTI a :: rest1, Label b :: IFNZRO c :: rest2) when a <> 0 -> let instrs2 = (replacePattern instrs [GOTO x] [GOTO c])
                                                                                       (replacePattern instrs2 [Label b] [INCSP 0], true)
                                                                                       //((List.rev (GOTO c :: rest1)) @ (IFNZRO c :: rest2), true)
            | _ -> ([], false)
        let rec optimizeJumps instrs jumpData =
            match jumpData with
            | (lab, 1) :: rest -> let beforeJump = findToBranch instrs lab
                                  let afterJump  = findAfterLabel instrs lab
                                  match optimizeJump instrs beforeJump afterJump with
                                  | (opti, true) -> optimizeJumps opti rest
                                  | (_, false) -> optimizeJumps instrs rest
            | (_, _) :: rest -> optimizeJumps instrs rest
            | [] -> instrs
        optimizeJumps allInstrs (Map.toList (findJumpData allInstrs Map.empty))

    let public optimizeBranching instrs =
        let optiInstrs = optimizeGoto instrs
        optimizeThroughBranches optiInstrs