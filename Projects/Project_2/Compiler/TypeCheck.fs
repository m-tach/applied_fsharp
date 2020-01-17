namespace GuardedCommands.Frontend
// Michael R. Hansen 06-01-2016 , 04-01-2018

open System
open Machine
open GuardedCommands.Frontend.AST

module TypeCheck = 

/// tcE gtenv ltenv e gives the type for expression e on the basis of type environments gtenv and ltenv
/// for global and local variables 
   let rec tcE gtenv ltenv  ex = 
      match ex with                        
      | N _              -> ITyp   
      | B _              -> BTyp   
      | STR _            -> CTyp 
      | Access acc       -> tcA gtenv ltenv acc    
      | Addr acc         -> PTyp (tcA gtenv ltenv acc)

      | PreInc acc       -> match tcA gtenv ltenv acc with
                            | ITyp -> ITyp
                            | _    -> failwith "preincrement expects an integer value"
      | PreDec acc       -> match tcA gtenv ltenv acc with
                            | ITyp -> ITyp
                            | _    -> failwith "predecrement expects an integer value"
                
      | Apply(f,[e]) when List.exists (fun x ->  x=f) ["-"; "!"; "len"]  
                         -> tcMonadic gtenv ltenv f e        

      | Apply(f,[e1;e2]) when List.exists (fun x ->  x=f) ["+";"-";"*"; "="; "&&"; "<>"; "<"; ">"; "<="; "||"]        
                         -> tcDyadic gtenv ltenv f e1 e2   

      | Apply(func, exps) when Map.containsKey func gtenv -> match Map.find func gtenv with
                                                             | FTyp(typs, Some(retType)) -> if exps.Length <> typs.Length then failwith ("function " + func + " expected " + (exps.Length).ToString() + " arguments but only " + (typs.Length).ToString() + " arguments were given")
                                                                                            let expTypes = List.map(fun x -> tcE gtenv ltenv x) exps
                                                                                            let normalizedExpTypes = List.map(fun x -> match x with
                                                                                                                                       | ATyp(t, _) -> ATyp(t, None)
                                                                                                                                       | _ -> x) expTypes
                                                                                            if not (List.forall(fun (x, y) -> x = y) (List.zip normalizedExpTypes typs)) then failwith (String.Format("Function has the type {0} but was given {1}", typs, normalizedExpTypes))
                                                                                            retType
                                                             | _ -> failwith "expected function but was not given a function"

      | _                -> failwith ("tcE: not supported yet " + ex.ToString())

   and tcMonadic gtenv ltenv f e = match (f, tcE gtenv ltenv e) with
                                   | ("-", ITyp) -> ITyp
                                   | ("!", BTyp) -> BTyp
                                   | ("len", ATyp(_, Some _)) -> ITyp
                                   | _           -> failwith("illegal/illtyped monadic expression: " + f)
   
   and tcDyadic gtenv ltenv f e1 e2 = match (f, tcE gtenv ltenv e1, tcE gtenv ltenv e2) with
                                      | (o, ITyp, ITyp) when List.exists (fun x ->  x=o) ["+";"*";"-"]  -> ITyp
                                      | (o, ITyp, ITyp) when List.exists (fun x ->  x=o) ["=";"<>";"<=";"<";">"] -> BTyp
                                      | (o, BTyp, BTyp) when List.exists (fun x ->  x=o) ["&&";"=";"<>";"||"]     -> BTyp 
                                      | (o, CTyp, CTyp) when List.exists (fun x ->  x=o) ["=";"<>"] -> BTyp 
                                      | _                      -> failwith("illegal/illtyped dyadic expression: " + f)

   and tcNaryFunction gtenv ltenv f es = failwith "type check: functions not supported yet"
 
   and tcNaryProcedure gtenv ltenv f es = failwith "type check: procedures not supported yet"
      

/// tcA gtenv ltenv e gives the type for access acc on the basis of type environments gtenv and ltenv
/// for global and local variables 
   and tcA gtenv ltenv ac = 
         match ac with 
         | AVar x         -> match Map.tryFind x ltenv with
                             | None   -> match Map.tryFind x gtenv with
                                         | None   -> failwith ("no declaration for : " + x)
                                         | Some t -> t
                             | Some t -> t            
         | AIndex(acc, e) -> match tcE gtenv ltenv e with
                              | ITyp -> match (tcA gtenv ltenv acc) with
                                          | ATyp (t,_) -> t
                                          | _ -> failwith "expected array but was not given an array"
                              | _    -> failwith "tcA: Array index has to be an int"

         | ADeref e       -> match tcE gtenv ltenv e with
                             | PTyp t  -> t
                             | _       -> failwith "not a pointer reference"
 

/// tcS gtenv ltenv retOpt s checks the well-typeness of a statement s on the basis of type environments gtenv and ltenv
/// for global and local variables and the possible type of return expressions 
   and tcS gtenv ltenv = function                           
                         | PrintLn e -> ignore(tcE gtenv ltenv e)
                         | Ass(acc,e) -> match tcA gtenv ltenv acc with
                                         | a when a = tcE gtenv ltenv e                  -> ()
                                         | ATyp(CTyp, _) when tcE gtenv ltenv e = CTyp   -> ()
                                         | _                                             -> failwith "illtyped assignment" 
                         | MAss(acc,e) -> if acc.Length <> e.Length then failwith (String.Format("Trying to multi assign {0} variables with {1} values", acc.Length, e.Length))
                                          let assignments = List.zip acc e
                                          List.iter (fun (cacc, ce) -> tcS gtenv ltenv (Ass(cacc, ce))) assignments
                                          ()

                         | Block([],stms)    -> List.iter (tcS gtenv ltenv) stms
                         | Block(decs, stms) -> List.iter (fun dec -> tcGDec gtenv dec |> ignore) decs
                                                let ltenv2 = Map.ofList ((List.map(fun dec -> match dec with
                                                                                              | VarDec(t, s) -> (s, t)
                                                                                              | _ -> failwith "local functions are not supported yet") decs) @ Map.toList ltenv)
                                                tcS gtenv ltenv2 (Block([], stms))

                         | Alt(GC(stms))   -> List.iter (fun (cexp, cstms) -> tcGC gtenv ltenv cexp cstms) stms
                                 
                         | Do(GC(stms))    -> List.iter (fun (cexp, cstms) -> tcGC gtenv ltenv cexp cstms) stms

                         | Return(Some(e)) -> tcE gtenv ltenv e |> ignore
                         | Return(_)       -> ()

                         | Call(func, exps) when Map.containsKey func gtenv  -> match Map.find func gtenv with
                                                                                | FTyp(typs, None) -> if exps.Length <> typs.Length then failwith ("function " + func + " expected " + (exps.Length).ToString() + " arguments but only " + (typs.Length).ToString() + " arguments were given")
                                                                                                      let expTypes = List.map(fun x -> tcE gtenv ltenv x) exps
                                                                                                      let normalizedExpTypes = List.map(fun x -> match x with
                                                                                                                                                 | ATyp(t, _) -> ATyp(t, None)
                                                                                                                                                 | _ -> x) expTypes
                                                                                                      if not (List.forall(fun (x, y) -> x = y) (List.zip normalizedExpTypes typs)) then failwith (String.Format("Function has the type {0} but was given {1}", typs, normalizedExpTypes))
                                                                                | _ -> failwith "expected a procedure but a procedure was not given"
                         | Call(func, exps) -> failwith ("The procedure " + func + " is not declared")                     

   and getReturnStms gtenv ltenv stm = 
      match stm with
      | PrintLn(_)        -> []
      | MAss(_)           -> []
      | Ass(_)            -> []
      | Return(Some(e))   -> [Some(tcE gtenv ltenv e)]
      | Return(_)         -> [None]
      | Alt(GC(eax))      -> List.collect(fun (_, stms) -> List.collect (getReturnStms gtenv ltenv) stms) eax
      | Do(GC(eax))       -> List.collect(fun (_, stms) -> List.collect (getReturnStms gtenv ltenv) stms) eax
      | Block(decs, stms) -> let gtenv2 = List.fold(fun map (x, y) -> Map.add x y map) gtenv (Map.toList ltenv)
                             let ltenv2 = List.fold tcGDec ltenv decs
                             List.collect (getReturnStms gtenv2 ltenv2) stms
      | Call(_)           -> []

   and tcGDec gtenv = function  
                      | VarDec(t,s)               -> Map.add s t gtenv
                      | FunDec(retTypOpt,f, decs, stm) -> let ltenv = List.fold tcGDec (Map.empty) decs
                                                          let gtenv2 = Map.add f (FTyp(List.map(fun dec -> match dec with
                                                                                                           | VarDec(t, _) -> t
                                                                                                           | _ -> failwith "function arguments can only be variables"
                                                                        ) decs, retTypOpt)) gtenv
                                                          let returnTypes = getReturnStms gtenv2 ltenv stm
                                                          if List.exists(fun actualRetTypOpt -> match actualRetTypOpt with
                                                                                                | Some(typ) when retTypOpt.IsSome -> retTypOpt.Value <> typ
                                                                                                | Some(_) -> failwith "procedure contain a return which returns a type"
                                                                                                | None when retTypOpt.IsNone -> false
                                                                                                | None -> failwith "function contains a return that doesn't return anything") returnTypes
                                                            then failwith "return type does not match functions expected return type"
                                                          tcS gtenv2 ltenv stm
                                                          gtenv2

   and tcGDecs gtenv = function
                       | dec::decs -> tcGDecs (tcGDec gtenv dec) decs
                       | _         -> gtenv

   /// tcGS checks if GuardedCommand has a valid condition type bool
   and tcGC gtenv ltenv cexp cstms = match (tcE gtenv ltenv cexp)  with
                                     | ITyp -> failwith "Illegal use of integer in alternative stm" 
                                     | _ -> List.iter (tcS gtenv ltenv) cstms 


/// tcP prog checks the well-typeness of a program prog
   and tcP(P(decs, stms)) = let gtenv = tcGDecs Map.empty decs
                            if (List.sumBy(fun x -> (getReturnStms gtenv Map.empty x).Length) stms) > 0 then failwith "can't return outside a function"
                            List.iter (tcS gtenv Map.empty) stms

  
