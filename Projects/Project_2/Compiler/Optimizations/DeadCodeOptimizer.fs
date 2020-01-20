namespace GuardedCommands.Backend

open System
open Machine

module DeadCodeAnalyzer =
    let rec private removeUntilLabel instrs =
        match instrs with
        | Label(a)::rest -> instrs
        | a::rest -> removeUntilLabel rest
        | [] -> []
    let rec private deadInstrElimination instrs =
        match instrs with
        | GOTO(a)::rest -> GOTO(a)::deadInstrElimination (removeUntilLabel rest)
        | RET(a)::rest -> RET(a)::deadInstrElimination (removeUntilLabel rest)
        | a::rest -> a::deadInstrElimination rest
        | [] -> []

    let rec private findJumpData instrs branches =
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

    let private removeUnusedLabels allInstrs =
        let rec removeUnusedLabelsInternal jumpData instrs =
            match instrs with
            | Label a :: rest when not (Map.containsKey a jumpData) -> removeUnusedLabelsInternal jumpData rest
            | a :: rest -> a :: removeUnusedLabelsInternal jumpData rest
            | [] -> []
        let jData = findJumpData allInstrs Map.empty
        removeUnusedLabelsInternal jData allInstrs

    let rec public deadCodeOptimization instrs =
        let withoutDeadCode = deadInstrElimination instrs
        let optiInstrs = removeUnusedLabels withoutDeadCode
        if optiInstrs.Length <> instrs.Length
        then deadCodeOptimization optiInstrs
        else optiInstrs