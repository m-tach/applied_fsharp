namespace GuardedCommands.Backend

open System
open Machine

module Optimizer =
    type InstrExp = Csti of int
                  | Add of InstrExp * InstrExp
                  | Sub of InstrExp * InstrExp
                  | Mul of InstrExp * InstrExp
                  | Div of InstrExp * InstrExp
                  | Mod of InstrExp * InstrExp
                  | Eq  of InstrExp * InstrExp
                  | Lt  of InstrExp * InstrExp
                  | Not of InstrExp
                  | Dup of InstrExp
                  | Ldi of InstrExp
                  | Sti of InstrExp * InstrExp
                  | Getbp
                  | Getsp
                  | Incsp of int
                  | Goto of label
                  | Ifzero of  label * InstrExp
                  | Ifnzro of label * InstrExp
                  | Call of int * label * InstrExp list
                  | Tcall of int * int * label * InstrExp list
                  | Ret of int *InstrExp
                  | Printi of InstrExp
                  | Printc of InstrExp
                  | Ldargs
                  | Stop
                  | Lab of label
                  | Nothing

    let private instrsToInstrsExp allintrs = 
        let rec inter intrs =
            match intrs with
            | CSTI(a)::rest1 -> (Csti(a), rest1)
            | ADD::rest1 -> let (e1, rest2) = inter rest1
                            let (e2, rest3) = inter rest2
                            (Add(e1, e2), rest3)
            | SUB::rest1 -> let (e1, rest2) = inter rest1
                            let (e2, rest3) = inter rest2
                            (Sub(e1, e2), rest3)
            | MUL::rest1 -> let (e1, rest2) = inter rest1
                            let (e2, rest3) = inter rest2
                            (Mul(e1, e2), rest3)
            | DIV::rest1 -> let (e1, rest2) = inter rest1
                            let (e2, rest3) = inter rest2
                            (Div(e1, e2), rest3)
            | MOD::rest1 -> let (e1, rest2) = inter rest1
                            let (e2, rest3) = inter rest2
                            (Mod(e1, e2), rest3)
            | EQ::rest1  -> let (e1, rest2) = inter rest1
                            let (e2, rest3) = inter rest2
                            (Eq(e1, e2), rest3)
            | LT::rest1  -> let (e1, rest2) = inter rest1
                            let (e2, rest3) = inter rest2
                            (Lt(e1, e2), rest3)         
            | NOT::rest1 -> let (e1, rest2) = inter rest1
                            (Not(e1), rest2)    
            | DUP::rest1 -> let (e1, rest2) = inter rest1
                            (Dup(e1), rest2)  
            | SWAP::rest1  -> failwith "optmizer can't handle swap instructions"
            | LDI::rest1 -> let (e1, rest2) = inter rest1
                            (Ldi(e1), rest2)     
            | STI::rest1  -> let (e1, rest2) = inter rest1
                             let (e2, rest3) = inter rest2
                             (Sti(e1, e2), rest3) 
            | GETBP::rest1 -> (Getbp, rest1)
            | GETSP::rest1 -> (Getsp, rest1)  
            | INCSP(a)::rest1 -> (Incsp(a), rest1)        
            | GOTO(a)::rest1 -> (Goto(a), rest1)
            | IFZERO(a)::rest1 -> let (e1, rest2) = inter rest1
                                  (Ifzero(a, e1), rest2) 
            | IFNZRO(a)::rest1 -> let (e1, rest2) = inter rest1
                                  (Ifnzro(a, e1), rest2)                    
            | CALL(m, a)::rest1 -> let (e1s, rest2) = List.fold(fun (es, rest) _ -> let (e1, rest2) = inter rest
                                                                                    (e1::es, rest2)) ([], rest1) [1..m]
                                   (Call(m, a, List.rev e1s), rest2)  
            | TCALL(m, n, a)::rest1 -> let (e1s, rest2) = List.fold(fun (es, rest) _ -> let (e1, rest2) = inter rest
                                                                                        (e1::es, rest2)) ([], rest1) [1..m]
                                       (Tcall(m, n, a, List.rev e1s), rest2)    
            | RET(m)::rest1 -> let (e1, rest2) = inter rest1
                               (Ret(m, e1), rest2)    
            | PRINTI::rest1 -> let (e1, rest2) = inter rest1
                               (Printi(e1), rest2)  
            | PRINTC::rest1 -> let (e1, rest2) = inter rest1
                               (Printc(e1), rest2)     
            | LDARGS::rest1 -> (Ldargs, rest1)    
            | STOP::rest1   -> (Stop, rest1)
            | Label(a)::rest1 -> (Lab(a), rest1)            
            | _ -> failwith "unexpected instruction"        

        let rec createExps dwa =
            match dwa with
                | (es, []) -> es
                | (es, rest) -> let (e, rest2) = inter rest
                                createExps (e::es, rest2)
        let (fstexp, rest) = inter (List.rev allintrs)
        createExps ([fstexp], rest)

    let rec private instrExpsToInstrs instrExps =
        let rec instrExpToInstrs instrExp =
            match instrExp with
            | Csti a -> [CSTI a]
            | Add(a, b) -> ADD::(List.collect instrExpToInstrs [a; b])
            | Sub(a, b) -> SUB::(List.collect instrExpToInstrs [a; b])
            | Mul(a, b) -> MUL::(List.collect instrExpToInstrs [a; b]) 
            | Div(a, b) -> DIV::(List.collect instrExpToInstrs [a; b])
            | Mod(a, b) -> MOD::(List.collect instrExpToInstrs [a; b])
            | Eq(a, b)  -> EQ:: (List.collect instrExpToInstrs [a; b]) 
            | Lt(a, b)  -> LT:: (List.collect instrExpToInstrs [a; b])
            | Not a -> NOT::instrExpToInstrs a
            | Dup a -> DUP::instrExpToInstrs a
            | Ldi(a) -> LDI::instrExpToInstrs a
            | Sti(a, b)  -> STI::(List.collect instrExpToInstrs [a; b])
            | Getbp -> [GETBP]
            | Getsp -> [GETSP]
            | Incsp a -> [INCSP a]
            | Goto a -> [GOTO a]
            | Ifzero(a, b) -> IFZERO a::instrExpToInstrs b
            | Ifnzro(a, b) -> IFNZRO a::instrExpToInstrs b
            | Call(a, b, c) -> CALL(a, b)::List.collect instrExpToInstrs c
            | Tcall(a, b, c, d) -> TCALL(a, b, c)::(List.collect instrExpToInstrs d)
            | Ret(a, b) -> RET a::instrExpToInstrs b
            | Printi a -> PRINTI::instrExpToInstrs a
            | Printc a -> PRINTC::instrExpToInstrs a
            | Ldargs -> [LDARGS]
            | Stop -> [STOP]
            | Lab a -> [Label a]
            | _ -> []        
        List.collect(fun x -> List.rev (instrExpToInstrs x)) instrExps


    let rec private removeUntilLabel instrs =
        match instrs with
        | Label(a)::rest -> instrs
        | a::rest -> removeUntilLabel rest
        | [] -> []
    let rec private deadInstrElimination instrs =
        match instrs with
        | GOTO(a)::rest -> GOTO(a)::deadInstrElimination (removeUntilLabel rest)
        | a::rest -> a::deadInstrElimination rest
        | [] -> []


    let rec private optimizeInstrExp intrExp = 
        //let recursed = if reachedBottom then intrExp else optimizeInstrExp intrExp        
        let recursed = match intrExp with
                       | Csti a -> Csti a
                       | Add(a, b) -> Add(optimizeInstrExp a, optimizeInstrExp b)
                       | Sub(a, b) -> Sub(optimizeInstrExp a, optimizeInstrExp b)
                       | Mul(a, b) -> Mul(optimizeInstrExp a, optimizeInstrExp b)
                       | Div(a, b) -> Div(optimizeInstrExp a, optimizeInstrExp b)
                       | Mod(a, b) -> Mod(optimizeInstrExp a, optimizeInstrExp b)
                       | Eq(a, b) -> Eq(optimizeInstrExp a, optimizeInstrExp b)
                       | Lt(a, b) -> Lt(optimizeInstrExp a, optimizeInstrExp b)
                       | Not(a) -> Not(optimizeInstrExp a)
                       | Dup(a) -> Dup(optimizeInstrExp a)
                       | Ldi(a) -> Ldi(optimizeInstrExp a)
                       | Sti(a, b) -> Sti(optimizeInstrExp a, optimizeInstrExp b)
                       | Getbp -> Getbp
                       | Getsp -> Getsp
                       | Incsp a -> Incsp a
                       | Goto a -> Goto a
                       | Ifzero(a, b) -> Ifzero(a, optimizeInstrExp b)
                       | Ifnzro(a, b) -> Ifnzro(a, optimizeInstrExp b)
                       | Call(a, b, c) -> Call(a, b, List.map optimizeInstrExp c)
                       | Tcall(a, b, c, d) -> Tcall(a, b, c, List.map optimizeInstrExp d)
                       | Ret(a, b) -> Ret(a, optimizeInstrExp b)
                       | Printi a -> Printi(optimizeInstrExp a)
                       | Printc a -> Printc(optimizeInstrExp a)
                       | Ldargs -> Ldargs
                       | Stop -> Stop
                       | Nothing -> Nothing
                       | Lab a -> Lab a
        match recursed with
        //Add
        | Add(Csti 0, b) -> b
        | Add(a, Csti 0) -> a
        | Add(Csti a, Csti b) -> Csti (a + b)
        //Sub
        | Sub(a, Csti 0) -> a
        | Sub(Csti a, Csti b) -> Csti (a - b)
        //Mul
        | Mul(Csti 0, _) -> Csti 0
        | Mul(_, Csti 0) -> Csti 0
        | Mul(Csti 1, b) -> b
        | Mul(a, Csti 1) -> a
        | Mul(Csti a, Csti b) -> Csti (a * b)
        //Div
        | Div(Csti 0, b) -> Csti 0
        | Div(a, Csti 1) -> a
        | Div(Csti a, Csti b) -> Csti (a / b)
        //Mod
        | Mod(Csti 0, b) -> Csti 0
        | Mod(Csti a, Csti b) -> Csti (a % b)
        //Eq
        | Eq(Csti a, Csti b) -> Csti (if a = b then 1 else 0)
        //Lt
        | Lt(Csti a, Csti b) -> Csti (if a < b then 1 else 0)
        //Not
        | Not(Csti 0) -> Csti 1
        | Not(Csti 1) -> Csti 0
        | Not(Not(a)) -> a
        //Ifzero
        | Ifzero(a, Csti 0) -> Goto a
        | Ifzero(a, Csti _) -> Nothing
        //Ifnzro
        | Ifnzro(a, Csti 0) -> Nothing
        | Ifnzro(a, Csti _) -> Goto a
        | _ -> recursed    

    let rec private optimizeInstrList instrs =
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

    let public instrsToString instrs = 
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

    let rec public optimize instrs = 
        let firstOptiInstrs = optimizeInstrList instrs
        let noDeadCode = deadInstrElimination firstOptiInstrs
        let exps = instrsToInstrsExp noDeadCode
        let optiExps = List.map optimizeInstrExp exps
        let optiInstrs = instrExpsToInstrs optiExps
        if instrs.Length <> optiInstrs.Length
        then optimize optiInstrs
        else optiInstrs                                   

