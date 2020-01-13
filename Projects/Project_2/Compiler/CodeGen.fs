namespace GuardedCommands.Backend
// Michael R. Hansen 05-01-2016, 04-01-2018
// This file is obtained by an adaption of the file MicroC/Comp.fs by Peter Sestoft
open System
open Machine

open GuardedCommands.Frontend.AST
module CodeGeneration =


(* A global variable has an absolute address, a local one has an offset: *)
   type Var = 
     | GloVar of int                   (* absolute address in stack           *)
     | LocVar of int                   (* address relative to bottom of frame *)

(* The variable environment keeps track of global and local variables, and 
   keeps track of next available offset for local variables *)

   type varEnv = Map<string, Var*Typ> * int

(* The function environment maps function name to label and parameter decs *)

   type ParamDecs = (Typ * string) list
   type funEnv = Map<string, label * Typ option * ParamDecs>

   let mergeParamWithEnv env pa = List.fold(fun (map, i) (t, s) -> varEnv(Map.add s (LocVar(i), t) map, i + 1)) env pa

/// CE vEnv fEnv e gives the code for an expression e on the basis of a variable and a function environment
   let rec CE (vEnv:varEnv) (fEnv:funEnv) expr = 
       match expr with
       | N n          -> [CSTI n]
       | B b          -> [CSTI (if b then 1 else 0)]
       | Access acc   -> CA vEnv fEnv acc @ [LDI] 

       | Apply("-", [e]) -> CE vEnv fEnv e @ [CSTI 0; SWAP; SUB]

       | Apply("!", [e]) -> CE vEnv fEnv e @ [NOT]

       | Apply("&&",[b1;b2]) -> let labend   = newLabel()
                                let labfalse = newLabel()
                                CE vEnv fEnv b1 @ [IFZERO labfalse] @ CE vEnv fEnv b2
                                @ [GOTO labend; Label labfalse; CSTI 0; Label labend]

       | Apply(o,[e1;e2]) when List.exists (fun x -> o=x) ["+"; "*"; "="; "-"]
                             -> let ins = match o with
                                          | "+"  -> [ADD]
                                          | "*"  -> [MUL]
                                          | "="  -> [EQ] 
                                          | "-"  -> [SUB] 
                                          | _    -> failwith "CE: this case is not possible"
                                CE vEnv fEnv e1 @ CE vEnv fEnv e2 @ ins
       
       | Apply(f, args) when Map.containsKey f fEnv -> let (label, typ, param) = Map.find f fEnv
                                                       (List.collect(fun x -> CE vEnv fEnv x) args) @ 
                                                       [CALL(param.Length, label)]
                                                       

       | _            -> failwith ("CE: not supported yet " + (expr.ToString()))
       

/// CA vEnv fEnv acc gives the code for an access acc on the basis of a variable and a function environment
   and CA vEnv fEnv = function | AVar x         -> match Map.find x (fst vEnv) with
                                                   | (GloVar addr,_) -> [CSTI addr]
                                                   | (LocVar addr,_) -> [CSTI addr; GETBP; ADD]
                               | AIndex(acc, e) -> failwith "CA: array indexing not supported yet" 
                               | ADeref e       -> failwith "CA: pointer dereferencing not supported yet"

  
(* Bind declared variable in env and generate code to allocate it: *)   
   let allocate (kind : int -> Var) (typ, x) (vEnv : varEnv)  =
    let (env, fdepth) = vEnv 
    match typ with
    | ATyp (ATyp _, _) -> 
      raise (Failure "allocate: array of arrays not permitted")
    | ATyp (t, Some i) -> failwith "allocate: array not supported yet"
    | _ -> 
      let newEnv = (Map.add x (kind fdepth, typ) env, fdepth+1)
      let code = [INCSP 1]
      (newEnv, code)

                      
/// CS vEnv fEnv s gives the code for a statement s on the basis of a variable and a function environment                          
   let rec CS (vEnv:varEnv) fEnv = function
       | PrintLn e        -> CE vEnv fEnv e @ [PRINTI; INCSP -1] 

       | Ass(acc,e)       -> CA vEnv fEnv acc @ CE vEnv fEnv e @ [STI; INCSP -1]

       | Return(Some(e))  -> let (_, locals) = vEnv
                             (CE vEnv fEnv e) @ [RET locals]

       | Block([],stms)          -> CSs vEnv fEnv stms
       | Block((VarDec(t, s))::tail,stms)   -> let (vEnv2, code) = allocate LocVar (t, s) vEnv
                                               code @ (CS vEnv2 fEnv (Block(tail, stms))) @ [INCSP -1]

       | MAss(acc,e)      -> List.collect (fun(cacc, ce) -> CS vEnv fEnv (Ass(cacc, ce))) (List.zip acc e)                                             


       | Alt(GC(stms))    -> let labend = newLabel()
                             List.foldBack (fun (cexp, cstms) state -> let lab = newLabel()
                                                                       CE vEnv fEnv cexp @ [IFZERO lab] @ CSs vEnv fEnv cstms @ [GOTO labend; Label lab] @ state) stms [Label labend]
       
       | Do(GC(stms))    -> let labstart = newLabel()
                            List.fold (fun state (cexp, cstms) -> let lab = newLabel()
                                                                  state @ CE vEnv fEnv cexp @ [IFZERO lab] @ CSs vEnv fEnv cstms @ [GOTO labstart; Label lab]) [Label labstart] stms

       | _                -> failwith "CS: this statement is not supported yet"

   and CSs vEnv fEnv stms = List.collect (CS vEnv fEnv) stms 



(* ------------------------------------------------------------------- *)

//type ParamDecs = (Typ * string) list
//type funEnv = Map<string, label * Typ option * ParamDecs>
//type varEnv = Map<string, Var*Typ> * int

(* Build environments for global variables and functions *)

   let makeGlobalEnvs decs = 
       let rec addv decs (vEnv:varEnv) fEnv = 
           match decs with 
           | []         -> (vEnv, fEnv, [])
           | dec::decr  -> 
             match dec with
             | VarDec (typ, var) -> let (vEnv1, code1) = allocate GloVar (typ, var) vEnv
                                    let (vEnv2, fEnv2, code2) = addv decr vEnv1 fEnv
                                    (vEnv2, fEnv2, code1 @ code2)
             | FunDec (typOpt, f, xs, body) -> let args = List.map(fun dec -> match dec with
                                                                              | VarDec(t, s) -> (t, s)
                                                                              | _ -> failwith "function arguments can only be variables") xs
                                               let functionStart = newLabel()
                                               let fEnv2 = Map.add f (functionStart, typOpt, args) fEnv
                                               let vEnv2 = (fst (mergeParamWithEnv vEnv args), args.Length)
                                               printfn "%s" (vEnv2.ToString())
                                               let funcCode = (Label functionStart)::(CS vEnv2 fEnv2 body)
                                               let (vEnv3, fEnv3, funcCode2) = addv decr vEnv fEnv2
                                               let skipFuncDec = newLabel()
                                               
                                               // function declarations are placed before the statements that should be run
                                               // so a goto is used to skip over the function code. This is only used when
                                               // the program starts.
                                               //
                                               // other declarations    GOTO    function code    label    rest of code
                                               //                        |                         ^
                                               //                        \-------------------------/

                                               (vEnv3, fEnv3, [GOTO skipFuncDec] @ funcCode @ [Label skipFuncDec] @ funcCode2)
       addv decs (Map.empty, 0) Map.empty

/// CP prog gives the code for a program prog
   let CP (P(decs,stms)) = 
       let _ = resetLabels ()
       let ((gvM,_) as gvEnv, fEnv, initCode) = makeGlobalEnvs decs
       initCode @ CSs gvEnv fEnv stms @ [STOP]     



