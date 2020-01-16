namespace GuardedCommands.Backend
// Michael R. Hansen 05-01-2016, 04-01-2018
// This file is obtained by an adaption of the file MicroC/Comp.fs by Peter Sestoft
open System
open Machine
open GuardedCommands.Frontend.TypeCheck

open GuardedCommands.Frontend.AST
module CodeGeneration =


(* A global variable has an absolute address, a local one has an offset: *)
   type Var = 
     | GloVar of int                   (* absolute address in stack           *)
     | LocVar of int                   (* address relative to bottom of frame *)

(* The variable environment keeps track of global and local variables, and 
   keeps track of next available offset for local variables 
   and whether this context is in a function*)

   type varEnv = Map<string, Var*Typ> * int * bool

(* The function environment maps function name to label and parameter decs *)

   type ParamDecs = (Typ * string) list
   type funEnv = Map<string, label * Typ option * ParamDecs>

/// CE vEnv fEnv e gives the code for an expression e on the basis of a variable and a function environment
   let rec CE (vEnv:varEnv) (fEnv:funEnv) expr = 
       match expr with
       | N n          -> [CSTI n]
       | B b          -> [CSTI (if b then 1 else 0)]
       | C c          -> List.map (fun c -> (CSTI (int c))) (Seq.toList c)
       | Access acc   -> CA vEnv fEnv acc @ [LDI] 
       | Addr acc     -> CA vEnv fEnv acc

       | Apply("-", [e]) -> CE vEnv fEnv e @ [CSTI 0; SWAP; SUB]

       | Apply("!", [e]) -> CE vEnv fEnv e @ [NOT]

       | Apply("len", ([Access(var)])) -> CA vEnv fEnv var @ [CSTI 1; SUB; LDI]

       | Apply("&&",[b1;b2]) -> let labend   = newLabel()
                                let labfalse = newLabel()
                                CE vEnv fEnv b1 @ [IFZERO labfalse] @ CE vEnv fEnv b2
                                @ [GOTO labend; Label labfalse; CSTI 0; Label labend]

       | Apply(o,[e1;e2]) when List.exists (fun x -> o=x) ["-"; "+"; "*"; "%"; "/"; "="; "<"; ">"; "<="; ">="; "<>"]
                             -> let ins = match o with
                                          | "-" ->  [SUB]
                                          | "+"  -> [ADD]
                                          | "*"  -> [MUL]
                                          | "/"  -> [DIV]
                                          | "%"  -> [MOD]
                                          | "="  -> [EQ]
                                          | "<>" -> [EQ; NOT]
                                          | "<"  -> [LT]
                                          | ">"  -> [SWAP; LT]
                                          | "<=" -> [SWAP; LT; NOT]
                                          | ">=" -> [LT; NOT] 
                                          | _    -> failwith "CE: this case is not possible"
                                CE vEnv fEnv e1 @ CE vEnv fEnv e2 @ ins
       
       | Apply(f, args) when Map.containsKey f fEnv -> let (label, typ, param) = Map.find f fEnv
                                                       (List.collect(fun x -> CE vEnv fEnv x) args) @ 
                                                       [CALL(param.Length, label)]
                                                       

       | _            -> failwith ("CE: not supported yet " + (expr.ToString()))
       

/// CA vEnv fEnv acc gives the code for an access acc on the basis of a variable and a function environment
   and CA vEnv fEnv = function | AVar x         -> let (map, _, _) = vEnv
                                                   match Map.find x map with
                                                   | (GloVar addr,_) -> [CSTI addr]
                                                   | (LocVar addr,_) -> [CSTI addr; GETBP; ADD]
                               | AIndex(acc, e) ->                                    
                                     CA vEnv fEnv acc @ CE vEnv fEnv e @ [ADD]

                               | ADeref e       -> CE vEnv fEnv e

  
(* Bind declared variable in env and generate code to allocate it: *)   
   let allocate (kind : int -> Var) (typ, x) (vEnv : varEnv)  =
    let (env, fdepth, isInFunc) = vEnv 
    match typ with
    | ATyp (ATyp _, _) -> 
      raise (Failure "allocate: array of arrays not permitted")

    | ATyp (t, Some i) when List.contains t [BTyp; ITyp; CTyp] -> 
      let newEnv = (Map.add x (kind (fdepth+1), typ) env, fdepth+i+1, isInFunc)
      let code = [CSTI i; INCSP i ] 
      (newEnv, code)
    | _ -> 
      let newEnv = (Map.add x (kind fdepth, typ) env, fdepth+1, isInFunc)
      let code = [INCSP 1]
      (newEnv, code)

   // Unpack varEnv and map it to (VarName -> VarType)
   let parseVarTypes (vars, _, _) = Map.map(fun _ v -> snd v) vars
   
   let printCTyp (str : list<instr>) : list<instr> =
      match (List.contains LDI str) with // For CTyp loaded from variables, we have the Load Indirect instruction. If we don't handle this, the List.fold would mess up our value-loading instructions.
      | true  -> str @ [PRINTC; INCSP -1; CSTI 10; PRINTC; INCSP -1] // CTyp is from a variable, so str is [CSTI arrLocation; CSTI index; ADD; LDI]
      | false -> (List.fold (fun s c -> match c with // CTyp is a literal, so str is [CSTI 74; CSTI 91; CSTI 95; ...] (ascii codes), and we append PRINTC to each char.
                                        | CSTI _ -> s @ [c; PRINTC; INCSP -1]
                                        | _      -> s @ [c]) [] str) @ [CSTI 10; PRINTC; INCSP -1] // We also append a newline (ascii 10, linux style) to the stack

/// CS vEnv fEnv s gives the code for a statement s on the basis of a variable and a function environment                          
   let rec CS (vEnv:varEnv) fEnv st = 
       match st with
       | PrintLn e        -> let tcVars = parseVarTypes vEnv
                             match tcE tcVars tcVars e with
                             | ATyp(CTyp, Some(len)) -> match e with // Typechecked expression is a char array; Get its address in the stack and iterate each char to print.
                                                        | Access acc -> let addr = CA vEnv fEnv acc
                                                                        (List.fold (fun s c -> s @ addr @ [CSTI c; ADD; LDI; PRINTC; INCSP -1]) [] [0 .. len]) @
                                                                        [CSTI 10; PRINTC; INCSP -1] // Also appends a newline (ascii 10, linux style) to the stack
                                                        | _          -> failwith "Impossible case: Array cannot be without a variable"
                             | CTyp _                -> printCTyp (CE vEnv fEnv e)
                                                        
                             | _                     -> CE vEnv fEnv e @ [PRINTI; INCSP -1] 

       | Ass(acc,e)       -> let tcVars = parseVarTypes vEnv
                             match tcE tcVars tcVars e with
                             | CTyp -> let str = CE vEnv fEnv e // str is the list of instructions of the string/char expression. [CSTI 65; CSTI 91; ...]
                                       match tcA tcVars tcVars acc with
                                       | ATyp(CTyp, _)            -> let addr = CA vEnv fEnv acc // Variable to assign to is a char array; for each character, store it in the array.
                                                                     snd (List.fold (fun (i, s) c -> match c with // The failwith-case should be impossible to get.
                                                                                                     | CSTI _ -> (i + 1, s @ addr @ [CSTI i; ADD; c; STI; INCSP -1])
                                                                                                     | _      -> failwith "Invalid syntax for string assignment."
                                                                                    ) (0, []) str)
                                       | CTyp when str.Length = 1 -> CA vEnv fEnv acc @ str @ [STI; INCSP -1] // Allows for char = char assignment. Single-char strings are also chars.
                                       | _                        -> failwith "Strings can only be assigned to char arrays."
                             | _    -> CA vEnv fEnv acc @ CE vEnv fEnv e @ [STI; INCSP -1]

       | Return(Some(e))  -> let (_, locals, _) = vEnv
                             (CE vEnv fEnv e) @ [RET locals]

       | Return(_)        -> let (_, locals, _) = vEnv
                             [CSTI 0; RET locals]                  

       | Block([],stms)          -> CSs vEnv fEnv stms
       | Block((VarDec(ATyp(t, Some len), s))::tail,stms)   -> let (_, _, isInFunc) = vEnv
                                                               let allocType = if isInFunc then LocVar else GloVar
                                                               let (vEnv2, code) = allocate allocType (t, s) vEnv
                                                               code @ (CS vEnv2 fEnv (Block(tail, stms))) @ [INCSP -(len + 1)]    
       | Block((VarDec(t, s))::tail,stms)   -> let (_, _, isInFunc) = vEnv
                                               let allocType = if isInFunc then LocVar else GloVar
                                               let (vEnv2, code) = allocate allocType (t, s) vEnv
                                               code @ (CS vEnv2 fEnv (Block(tail, stms))) @ [INCSP -1]
       | Block(_) -> failwith "local functions are not supported"                                             

       | MAss(acc,e)      -> List.collect (fun(cacc, ce) -> CS vEnv fEnv (Ass(cacc, ce))) (List.zip acc e)                                             


       | Alt(GC(stms))    -> let labend = newLabel()
                             List.foldBack (fun (cexp, cstms) state -> let lab = newLabel()
                                                                       CE vEnv fEnv cexp @ [IFZERO lab] @ CSs vEnv fEnv cstms @ [GOTO labend; Label lab] @ state) stms [Label labend]
       
       | Do(GC(stms))    -> let labstart = newLabel()
                            List.fold (fun state (cexp, cstms) -> let lab = newLabel()
                                                                  state @ CE vEnv fEnv cexp @ [IFZERO lab] @ CSs vEnv fEnv cstms @ [GOTO labstart; Label lab]) [Label labstart] stms

       | Call(f, args) when Map.containsKey f fEnv -> let (label, _, param) = Map.find f fEnv
                                                      (List.collect(fun x -> CE vEnv fEnv x) args) @ 
                                                      [CALL(param.Length, label); INCSP -1]
       | Call(_) -> failwith "expected a procedure but did not get one"

   and CSs vEnv fEnv stms = List.collect (CS vEnv fEnv) stms 



(* ------------------------------------------------------------------- *)


(* Build environments for global variables and functions *)

   let makeGlobalEnvs decs: (varEnv * funEnv * instr list) = 
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
                                               let (vmap, _, _) = vEnv
                                               let vEnv2 = (fst (List.fold(fun (map, i) (t, s) -> (Map.add s (LocVar(i), t) map, i + 1)) (vmap, 0) args), args.Length, true)
                                               let funcCode = (Label functionStart)::(CS vEnv2 fEnv2 body)
                                               let (vEnv3, fEnv3, funcCode2) = addv decr vEnv fEnv2
                                               let skipFuncDec = newLabel()
                                               
                                               // function declarations are placed before the statements that should be run
                                               // so a goto is used to skip over the function code. This is only used when
                                               // the program starts.
                                               //
                                               // other declarations    GOTO    function code    implicit return    label    rest of code
                                               //                        |                                            ^
                                               //                        \--------------------------------------------/

                                               let implicitReturn = if typOpt.IsNone then [CSTI 0; RET xs.Length] else []
                                               (vEnv3, fEnv3, [GOTO skipFuncDec] @ funcCode @ implicitReturn @ [Label skipFuncDec] @ funcCode2)
       addv decs (Map.empty, 0, false) Map.empty

/// CP prog gives the code for a program prog
   let CP (P(decs,stms)) = 
       let _ = resetLabels ()
       let ((map, i, _), fEnv, initCode) = makeGlobalEnvs decs
       initCode @ CSs (map, i, false) fEnv stms @ [STOP]     



