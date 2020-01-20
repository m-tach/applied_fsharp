namespace Tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

open GuardedCommands.Util
open GuardedCommands.Frontend.TypeCheck
open GuardedCommands.Frontend.AST
open GuardedCommands.Backend.CodeGeneration
open GuardedCommands.Backend

open ParserUtil
open CompilerUtil
open Machine

[<TestClass>]
type TestOperator () =

    let runInstrs instrs =
        Array.toList(VirtualMachine.execcode(List.toArray (Machine.code2ints instrs), (Array.zeroCreate 10000), (Array.zeroCreate 0)) false)


    let verifyBIExpr typ oper =
        let program = String.Format("begin x: {0}, y: {0}, z: {0}; z := x {1} y end", typ, oper)
        try
            let ast = parseString program
            tcP ast
        with
        | e -> failwith (String.Format("Program:\n{0}\n\nFailed with: {1}\n\nStacktrace:\n", program, e.Message, e.StackTrace)) 

    let verifyBIExprToBool typ oper =
        let program = String.Format("begin x: {0}, y: {0}, z: bool; z := x {1} y end", typ, oper)
        try
            let ast = parseString program
            tcP ast  
        with
        | e -> failwith (String.Format("Program:\n{0}\n\nFailed with: {1}\n\nStacktrace:\n", program, e.Message, e.StackTrace)) 
    let verifyMonoPreExprToBool typ oper =
        let program = String.Format("begin x: {0}, z: bool; z := {1} x end", typ, oper)
        try
            let ast = parseString program
            tcP ast
        with
        | e -> failwith (String.Format("Program:\n{0}\n\nFailed with: {1}\n\nStacktrace:\n", program, e.Message, e.StackTrace)) 
    let verifyMonoPreExprDualTyp typ1 typ2 oper =
        let program = String.Format("begin x: {0}, z: {1}; z := {2} x end", typ1, typ2, oper)
        try
            let ast = parseString program
            tcP ast
        with
        | e -> failwith (String.Format("Program:\n{0}\n\nFailed with: {1}\n\nStacktrace:\n", program, e.Message, e.StackTrace))       
    let verifyMonoPostExprToBool typ oper =
        let program = String.Format("begin x: {0}, z: bool; z := x {1} end", typ, oper)
        try
            let ast = parseString program
            tcP ast
        with
        | e -> failwith (String.Format("Program:\n{0}\n\nFailed with: {1}\n\nStacktrace:\n", program, e.Message, e.StackTrace))     
    let verifyMonoPostExprDualTyp typ1 typ2 oper =
        let program = String.Format("begin x: {0}, z: {1}; z :=  x {2} end", typ1, typ2, oper)
        try
            let ast = parseString program
            tcP ast
        with
        | e -> failwith (String.Format("Program:\n{0}\n\nFailed with: {1}\n\nStacktrace:\n", program, e.Message, e.StackTrace))      

    [<TestMethod>]
    member this.IntPlus () = verifyBIExpr "int" "+"
    [<TestMethod>]
    member this.IntMinus () = verifyBIExpr "int" "-"
    [<TestMethod>]
    member this.IntMult () = verifyBIExpr "int" "*"
    [<TestMethod>]
    member this.IntDiv () = verifyBIExpr "int" "/"
    [<TestMethod>]
    member this.IntEq () = verifyBIExprToBool "int" "="
    [<TestMethod>]
    member this.IntLess () = verifyBIExprToBool "int" "<"
    [<TestMethod>]
    member this.IntLeq () = verifyBIExprToBool "int" "<="
    [<TestMethod>]
    member this.IntGeq () = verifyBIExprToBool "int" ">="
    [<TestMethod>]
    member this.IntGreater () = verifyBIExprToBool "int" ">"
    [<TestMethod>]
    member this.IntNeq () = verifyBIExprToBool "int" "<>"
    [<TestMethod>]
    member this.IntNot () = Assert.ThrowsException(fun _ -> verifyMonoPreExprToBool "int" "!") |> ignore
    [<TestMethod>]
    member this.IntLength () = Assert.ThrowsException(fun _ -> verifyMonoPreExprDualTyp "int" "int" "len") |> ignore
    [<TestMethod>]
    member this.IntDeref () = Assert.ThrowsException(fun _ -> verifyMonoPostExprDualTyp "int" "int" "^") |> ignore
    [<TestMethod>]
    member this.IntAddress () = verifyMonoPreExprDualTyp "int" "^int" "&"

    [<TestMethod>]
    member this.BoolPlus () = Assert.ThrowsException(fun _ -> verifyBIExpr "bool" "+") |> ignore
    [<TestMethod>]
    member this.BoolMinus () = Assert.ThrowsException(fun _ -> verifyBIExpr "bool" "-") |> ignore
    [<TestMethod>]
    member this.BoolMult () = Assert.ThrowsException(fun _ -> verifyBIExpr "bool" "*") |> ignore
    [<TestMethod>]
    member this.BoolDiv () = Assert.ThrowsException(fun _ -> verifyBIExpr "bool" "/") |> ignore
    [<TestMethod>]
    member this.BoolEq () = verifyBIExprToBool "bool" "="
    [<TestMethod>]
    member this.BoolLess () = Assert.ThrowsException(fun _ -> verifyBIExprToBool "bool" "<") |> ignore
    [<TestMethod>]
    member this.BoolLeq () = Assert.ThrowsException(fun _ -> verifyBIExprToBool "bool" "<=") |> ignore
    [<TestMethod>]
    member this.BoolGeq () = Assert.ThrowsException(fun _ -> verifyBIExprToBool "bool" ">=") |> ignore
    [<TestMethod>]
    member this.BoolGreater () = Assert.ThrowsException(fun _ -> verifyBIExprToBool "bool" ">") |> ignore
    [<TestMethod>]
    member this.BoolNeq () = verifyBIExprToBool "bool" "<>"
    [<TestMethod>]
    member this.BoolNot () = verifyMonoPreExprToBool "bool" "!"
    [<TestMethod>]
    member this.BoolLength () = Assert.ThrowsException(fun _ -> verifyMonoPreExprDualTyp "bool" "int" "len") |> ignore
    [<TestMethod>]
    member this.BoolDeref () = Assert.ThrowsException(fun _ -> verifyMonoPostExprDualTyp "bool" "bool" "^") |> ignore
    [<TestMethod>]
    member this.BoolAddress () = verifyMonoPreExprDualTyp "bool" "^bool" "&"

    [<TestMethod>]
    member this.CharPlus () = Assert.ThrowsException(fun _ -> verifyBIExpr "char" "+") |> ignore
    [<TestMethod>]
    member this.CharMinus () = Assert.ThrowsException(fun _ -> verifyBIExpr "char" "-") |> ignore
    [<TestMethod>]
    member this.CharMult () = Assert.ThrowsException(fun _ -> verifyBIExpr "char" "*") |> ignore
    [<TestMethod>]
    member this.CharDiv () = Assert.ThrowsException(fun _ -> verifyBIExpr "char" "/") |> ignore
    [<TestMethod>]
    member this.CharEq () = verifyBIExprToBool "char" "="
    [<TestMethod>]
    member this.CharLess () = Assert.ThrowsException(fun _ -> verifyBIExprToBool "char" "<") |> ignore
    [<TestMethod>]
    member this.CharLeq () = Assert.ThrowsException(fun _ -> verifyBIExprToBool "char" "<=") |> ignore
    [<TestMethod>]
    member this.CharGeq () = Assert.ThrowsException(fun _ -> verifyBIExprToBool "char" ">=") |> ignore
    [<TestMethod>]
    member this.CharGreater () = Assert.ThrowsException(fun _ -> verifyBIExprToBool "char" ">") |> ignore
    [<TestMethod>]
    member this.CharNeq () = verifyBIExprToBool "char" "<>"
    [<TestMethod>]
    member this.CharNot () = Assert.ThrowsException(fun _ -> verifyMonoPreExprToBool "char" "!") |> ignore
    [<TestMethod>]
    member this.CharLength () = Assert.ThrowsException(fun _ -> verifyMonoPreExprDualTyp "char" "int" "len") |> ignore
    [<TestMethod>]
    member this.CharDeref () = Assert.ThrowsException(fun _ -> verifyMonoPostExprDualTyp "char" "char" "^") |> ignore
    [<TestMethod>]
    member this.CharAddress () = verifyMonoPreExprDualTyp "char" "^char" "&"

    [<TestMethod>]
    member this.ArrayPlus () = Assert.ThrowsException(fun _ -> verifyBIExpr "int[2]" "+") |> ignore
    [<TestMethod>]
    member this.ArrayMinus () = Assert.ThrowsException(fun _ -> verifyBIExpr "int[2]" "-") |> ignore
    [<TestMethod>]
    member this.ArrayMult () = Assert.ThrowsException(fun _ -> verifyBIExpr "int[2]" "*") |> ignore
    [<TestMethod>]
    member this.ArrayDiv () = Assert.ThrowsException(fun _ -> verifyBIExpr "int[2]" "/") |> ignore
    [<TestMethod>]
    member this.ArrayEq () = Assert.ThrowsException(fun _ -> verifyBIExprToBool "int[2]" "=") |> ignore
    [<TestMethod>]
    member this.ArrayLess () = Assert.ThrowsException(fun _ -> verifyBIExprToBool "int[2]" "<") |> ignore
    [<TestMethod>]
    member this.ArrayLeq () = Assert.ThrowsException(fun _ -> verifyBIExprToBool "int[2]" "<=") |> ignore
    [<TestMethod>]
    member this.ArrayGeq () = Assert.ThrowsException(fun _ -> verifyBIExprToBool "int[2]" ">=") |> ignore
    [<TestMethod>]
    member this.ArrayGreater () = Assert.ThrowsException(fun _ -> verifyBIExprToBool "int[2]" ">") |> ignore
    [<TestMethod>]
    member this.ArrayNeq () = Assert.ThrowsException(fun _ -> verifyBIExprToBool "int[2]" "<>") |> ignore
    [<TestMethod>]
    member this.ArrayNot () = Assert.ThrowsException(fun _ -> verifyMonoPreExprToBool "int[2]" "!") |> ignore
    [<TestMethod>]
    member this.ArrayLength () = verifyMonoPreExprDualTyp "int[2]" "int" "len"
    [<TestMethod>]
    member this.ArrayDeref () = Assert.ThrowsException(fun _ -> verifyMonoPostExprDualTyp "int[2]" "int[2]" "^") |> ignore
    [<TestMethod>]
    member this.ArrayAddress () = verifyMonoPreExprDualTyp "int[2]" "^int[]" "&"

    [<TestMethod>]
    member this.IntPtrPlus () = Assert.ThrowsException(fun _ -> verifyBIExpr "^int" "+") |> ignore
    [<TestMethod>]
    member this.IntPtrMinus () = Assert.ThrowsException(fun _ -> verifyBIExpr "^int" "-") |> ignore
    [<TestMethod>]
    member this.IntPtrMult () = Assert.ThrowsException(fun _ -> verifyBIExpr "^int" "*") |> ignore
    [<TestMethod>]
    member this.IntPtrDiv () = Assert.ThrowsException(fun _ -> verifyBIExpr "^int" "/") |> ignore
    [<TestMethod>]
    member this.IntPtrEq () = Assert.ThrowsException(fun _ -> verifyBIExprToBool "^int" "=") |> ignore
    [<TestMethod>]
    member this.IntPtrLess () = Assert.ThrowsException(fun _ -> verifyBIExprToBool "^int" "<") |> ignore
    [<TestMethod>]
    member this.IntPtrLeq () = Assert.ThrowsException(fun _ -> verifyBIExprToBool "^int" "<=") |> ignore
    [<TestMethod>]
    member this.IntPtrGeq () = Assert.ThrowsException(fun _ -> verifyBIExprToBool "^int" ">=") |> ignore
    [<TestMethod>]
    member this.IntPtrGreater () = Assert.ThrowsException(fun _ -> verifyBIExprToBool "^int" ">") |> ignore
    [<TestMethod>]
    member this.IntPtrNeq () = Assert.ThrowsException(fun _ -> verifyBIExprToBool "^int" "<>") |> ignore
    [<TestMethod>]
    member this.IntPtrNot () = Assert.ThrowsException(fun _ -> verifyMonoPreExprToBool "^int" "!") |> ignore
    [<TestMethod>]
    member this.IntPtrLength () = Assert.ThrowsException(fun _ -> verifyMonoPreExprDualTyp "^int" "int" "len") |> ignore
    [<TestMethod>]
    member this.IntPtrDeref () = verifyMonoPostExprDualTyp "^int" "int" "^"
    [<TestMethod>]
    member this.IntPtrAddress () = verifyMonoPreExprDualTyp "^int" "^^int" "&"