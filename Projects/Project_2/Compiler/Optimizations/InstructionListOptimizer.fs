namespace GuardedCommands.Backend

open System
open Machine

module InstructionListAnalyzer =
    let rec public optimizeInstrList instrs =
        match instrs with
        //Dup
        | DUP :: INCSP a :: rest when a < 0 -> INCSP (a - 1) :: optimizeInstrList rest
        | DUP :: RET a :: rest when a < 0 -> RET (a - 1) :: optimizeInstrList rest
        //Incsp
        | INCSP a :: INCSP b :: rest -> INCSP (a + b) :: optimizeInstrList rest
        | GOTO a :: Label b :: rest when a = b -> optimizeInstrList rest
        | CALL(m, a):: RET n :: rest -> RET n :: TCALL(m, n, a) :: optimizeInstrList rest
        | a :: rest -> a :: optimizeInstrList rest
        | [] -> []