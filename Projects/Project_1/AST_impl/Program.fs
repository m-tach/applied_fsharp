namespace ASTConverter

open System
open TreeDesign

    type Exp =                            
             | N  of int                   (* Integer constant            *)
             | B of bool                   (* Boolean constant            *)
             | Access of Access            (* x    or  ^p    or  a[e]     *)
             | Addr of Access              (* &x   or  &p^   or  &a[e]    *)
             | Apply of string * Exp list  (* Function application        *)
             
    and Access = 
              | AVar of string             (* Variable access        x    *) 
              | AIndex of Access * Exp     (* Array indexing         a[e] *)
              | ADeref of Exp              (* Pointer dereferencing  p^   *)

    type Stm  =                            
              | PrintLn of Exp               (* Print                          *) 
              | Ass of Access * Exp          (* x:=e  or  p^:=e  or  a[e]:=e   *)
              | Return of Exp option         (* Return from function           *)   
              | Alt of GuardedCommand        (* Alternative statement          *) 
              | Do of GuardedCommand         (* Repetition statement           *) 
              | Block of Dec list * Stm list (* Block: grouping and scope      *)
              | Call of string * Exp list    (* Procedure call                 *)
                   
    and GuardedCommand = GC of (Exp * Stm list) list (* Guarded commands    *)

    and Dec = 
             | VarDec of Typ * string        (* Variable declaration               *)
             | FunDec of Typ option * string * Dec list * Stm
                                             (* Function and procedure declaration *) 

    and Typ  = 
             | ITyp                          (* Type int                    *)
             | BTyp                          (* Type bool                   *)
             | ATyp of Typ * int option      (* Type array                  *)
             | PTyp of Typ                   (* Type pointer                *)
             | FTyp of Typ list * Typ option (* Type function and procedure *)

    type Program = P of Dec list * Stm list   (* Program                 *)

module ASTConverter =

    let rec public programToTree (P(decs, stms)) = Node("Program", [decsToNodes decs; stmsToNodes stms])

    and private decsToNodes decs = match decs with
                                   | [dec] -> decToNode dec
                                   | _ -> Node("Declarations", List.map decToNode decs)
    and private decToNode dec = match dec with
                                | VarDec(t, s) -> Node("Variable declaration", [typToNode(t); Node(s, [])])
                                | FunDec(Some(t), s, decs, stm) -> Node("Function  declaration", [typToNode t; Node(s, []); decsToNodes decs; stmToNode stm])
                                | FunDec(None, s, decs, stm) -> Node("Function  declaration", [Node(s, []); decsToNodes decs; stmToNode stm])

    and private typToNode typ = match typ with 
                                | ITyp -> Node("int", [])
                                | BTyp -> Node("bool", [])
                                | ATyp(t, Some(x)) -> Node("Array", [typToNode(t); Node(String.Format("Size: {0}", x), [])])
                                | ATyp(t, None) -> Node("Array", [typToNode(t); Node(String.Format("Size: {0}", "Unknown"), [])])
                                | PTyp(t) -> Node("Pointer", [typToNode t])
                                //don't understand what types and t are in this context. Do they define a function call?
                                | FTyp(types, Some(t)) -> Node("Function", (List.map typToNode types) @ [typToNode t])
                                | FTyp(types, None) -> Node("Function", (List.map typToNode types)(* @ [typToNode t]*))

    and private stmsToNodes stms = match stms with
                                   | [stm] -> stmToNode stm
                                   | _ -> Node("Statements", List.map stmToNode stms)
    and private stmToNode stm = match stm with
                                | PrintLn(e) -> Node("PrintLn",[expToNode e])
                                | Ass(a, e) -> Node("Assignment", [accToNode a; expToNode e])
                                | Return(Some(e)) -> Node("Return", [expToNode e])
                                | Return(None) -> Node("Return", [])
                                //What's an alt?
                                | Alt(gc) -> Node("Alt", guardToNodeList gc)
                                | Do(gc) -> Node("Do", guardToNodeList gc)
                                | Block(decs, stms) -> Node("Block", [decsToNodes decs; stmsToNodes stms])
                                | Call(s, [])  -> Node("Function call", [Node(s, []);])
                                | Call(s, [e]) -> Node("Function call", [Node(s, []); Node("Argument", [expToNode e])])
                                | Call(s, e)   -> Node("Function call", [Node(s, []); Node("Arguments", List.map expToNode e)])

    and private expToNode exp = match exp with
                                | N(n) -> Node(n.ToString(), [])
                                | B(b) -> Node(b.ToString(), [])
                                | Access(AVar(acc)) -> accToNode (AVar(acc))
                                | Access(a) -> Node("Access", [accToNode a])
                                | Addr(a) -> Node("Address", [accToNode a])
                                //Is it correct that this is a function call
                                | Apply(s, [])  -> Node("Operator", [Node(s, []);])
                                | Apply(s, [e]) -> Node("Operator", [Node(s, []); Node("Argument", [expToNode e])])
                                | Apply(s, e)   -> Node("Operator", [Node(s, []); Node("Arguments", List.map expToNode e)])

    and private accToNode acc = match acc with
                                | AVar(s) -> Node(s, [])
                                | AIndex(a, e) -> Node("Index", [accToNode a; expToNode e])
                                | ADeref(e) -> Node("Deref", [expToNode e])

    //no clue what this is for either
    and private guardToNodeList (GC(stuff)) = List.map(fun (x, y) -> Node("guarded", [expToNode x; stmsToNodes y])) stuff