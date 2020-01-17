namespace GuardedCommands.Backend

open System
open Machine

module DeadCodeAnalyzer =
    let rec private removeUntilLabel instrs =
        match instrs with
        | Label(a)::rest -> instrs
        | a::rest -> removeUntilLabel rest
        | [] -> []
    let rec public deadInstrElimination instrs =
        match instrs with
        | GOTO(a)::rest -> GOTO(a)::deadInstrElimination (removeUntilLabel rest)
        | a::rest -> a::deadInstrElimination rest
        | [] -> []