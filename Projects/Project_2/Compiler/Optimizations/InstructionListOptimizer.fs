namespace GuardedCommands.Backend

open System
open Machine

module InstructionListAnalyzer =
    let rec public optimizeInstrList allInstrs =
        let rec optimizeInstrListInternal instrs =
            match instrs with
            //Dup
            | DUP :: INCSP a :: rest when a < 0 -> INCSP (a - 1) :: optimizeInstrList rest
            | DUP :: RET a :: rest when a < 0 -> RET (a - 1) :: optimizeInstrList rest
            //Incsp
            | INCSP a :: INCSP b :: rest -> INCSP (a + b) :: optimizeInstrList rest
            | GOTO a :: Label b :: rest when a = b -> optimizeInstrList rest
            | GOTO a :: GOTO b :: rest when a = b -> printfn "%s" "adaw" 
                                                     GOTO a :: optimizeInstrList rest
            | CALL(m, a):: RET n :: rest -> TCALL(m, n, a) :: RET 0 :: optimizeInstrList rest
            | a :: rest -> a :: optimizeInstrList rest
            | [] -> []
        let optiInstrs = optimizeInstrListInternal allInstrs
        if optiInstrs.Length <> allInstrs.Length
        then optimizeInstrList optiInstrs
        else optiInstrs