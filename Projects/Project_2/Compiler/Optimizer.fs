namespace GuardedCommands.Backend
// Michael R. Hansen 05-01-2016, 04-01-2018
// This file is obtained by an adaption of the file MicroC/Comp.fs by Peter Sestoft
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
                  | Swap of InstrExp * InstrExp
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
            | SWAP::rest1  -> let (e1, rest2) = inter rest1
                              let (e2, rest3) = inter rest2
                              (Swap(e1, e2), rest3)   
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
        let reachedBottom = 
            match intrExp with
            | Csti(_) -> true
            | Getbp(_) -> true
            | Getsp(_) -> true
            | Incsp(_) -> true
            | Goto(_) -> true
            | Ldargs(_) -> true
            | Stop(_) -> true
            | _ -> false
        let recursed = if reachedBottom then intrExp else optimizeInstrExp intrExp        
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
        //Swap
        //
        | CSTI a :: CSTI b :: SWAP :: rest -> CSTI b :: CSTI a :: optimizeInstrs rest
        
        
        
        | CSTI a :: DIV :: rest -> optimizeInstrs rest
        | CSTI a :: MOD :: rest -> optimizeInstrs rest
        | [] -> []
        | a :: rest -> a :: optimizeInstrs rest

    let rec private optimizeInstrList instrs =
        match instrs with
        //Dup
        | DUP :: INCSP a :: rest when a < 0 -> INCSP (a - 1) :: optimizeInstrList rest
        | DUP :: RET a :: rest when a < 0 -> RET (a - 1) :: optimizeInstrList rest
        //Swap
        | 

    let optimize instrs = let noDeadCode = deadInstrElimination instrs    
                          let exps = instrsToInstrsExp noDeadCode

