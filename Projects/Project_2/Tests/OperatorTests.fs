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
            runInstrs (CP ast) |> ignore
        with
        | e -> failwith (String.Format("Program:\n{0}\n\nFailed with: {1}\n\nStacktrace:\n", program, e.Message, e.StackTrace)) 

    let verifyBIExprToBool typ oper =
        let program = String.Format("begin x: {0}, y: {0}, z: bool; z := x {1} y end", typ, oper)
        try
            let ast = parseString program
            runInstrs (CP ast) |> ignore   
        with
        | e -> failwith (String.Format("Program:\n{0}\n\nFailed with: {1}\n\nStacktrace:\n", program, e.Message, e.StackTrace)) 
    let verifyMonoPreExprToBool typ oper =
        let program = String.Format("begin x: {0}, z: bool; z := {1} x end", typ, oper)
        try
            let ast = parseString program
            runInstrs (CP ast) |> ignore 
        with
        | e -> failwith (String.Format("Program:\n{0}\n\nFailed with: {1}\n\nStacktrace:\n", program, e.Message, e.StackTrace)) 
    let verifyMonoPreExprDualTyp typ1 typ2 oper =
        let program = String.Format("begin x: {0}, z: {1}; z := {2} x end", typ1, typ2, oper)
        try
            let ast = parseString program
            runInstrs (CP ast) |> ignore
        with
        | e -> failwith (String.Format("Program:\n{0}\n\nFailed with: {1}\n\nStacktrace:\n", program, e.Message, e.StackTrace))       
    let verifyMonoPostExprToBool typ oper =
        let program = String.Format("begin x: {0}, z: bool; z := x {1} end", typ, oper)
        try
            let ast = parseString program
            runInstrs (CP ast) |> ignore 
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
    member this.IntEq () = verifyBIExpr "int" "="
    [<TestMethod>]
    member this.IntLess () = verifyBIExpr "int" "<"
    [<TestMethod>]
    member this.IntLeq () = verifyBIExpr "int" "<="
    [<TestMethod>]
    member this.IntGeq () = verifyBIExpr "int" ">="
    [<TestMethod>]
    member this.IntGreater () = verifyBIExpr "int" ">"
    [<TestMethod>]
    member this.IntNeq () = verifyBIExpr "int" "<>"
    [<TestMethod>]
    member this.IntNot () = verifyMonoPreExprToBool "int" "!"
    [<TestMethod>]
    member this.IntLength () = verifyMonoPreExprToBool "int" "len"
    [<TestMethod>]
    member this.IntDeref () = verifyMonoPostExprToBool "int" "^"
    [<TestMethod>]
    member this.IntAddress () = verifyMonoPreExprDualTyp "int" "^int" "&"