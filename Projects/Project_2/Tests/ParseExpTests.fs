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
open InstructionExpAnalyzer

[<TestClass>]
type TestParseExp () =
    let verifyParse given expected =
        let actual = instrsToInstrsExp given
        Assert.AreEqual(expected, actual)
        let andBack = instrExpsToInstrs actual
        Assert.AreEqual(given, andBack)

    [<TestMethod>]
    member this.ParseExp1 () =
        verifyParse [CSTI 1; CSTI 2; ADD; CSTI 3; CSTI 1; SUB; MUL] [Mul(Add(Csti 1, Csti 2), Sub(Csti 3, Csti 1))]

    [<TestMethod>]
    member this.ParseExpAdd () =
        verifyParse [CSTI 3; CSTI 4; ADD] [Add(Csti 3, Csti 4)]  

    [<TestMethod>]
    member this.ParseExpSub () =
        verifyParse [CSTI 3; CSTI 4; SUB] [Sub(Csti 3, Csti 4)]

    [<TestMethod>]
    member this.ParseExpMul () =
        verifyParse [CSTI 3; CSTI 4; MUL] [Mul(Csti 3, Csti 4)]

    [<TestMethod>]
    member this.ParseExpDiv () =
        verifyParse [CSTI 3; CSTI 4; DIV] [Div(Csti 3, Csti 4)]

    [<TestMethod>]
    member this.ParseExpMod () =
        verifyParse [CSTI 3; CSTI 4; MOD] [Mod(Csti 3, Csti 4)]

    [<TestMethod>]
    member this.ParseExpEq () =
        verifyParse [CSTI 3; CSTI 4; EQ ] [Eq(Csti 3, Csti 4)]

    [<TestMethod>]
    member this.ParseExpLt () =
        verifyParse [CSTI 3; CSTI 4; LT ] [Lt(Csti 3, Csti 4)]    

    [<TestMethod>]
    member this.ParseExpNot () =
        verifyParse [CSTI 1; NOT ] [Not(Csti 1)] 
        
    [<TestMethod>]
    member this.ParseExpDup () =
        verifyParse [CSTI 1; DUP ] [Dup(Csti 1)]  

    [<TestMethod>]
    member this.ParseExpSwap () =
        Assert.ThrowsException(fun _ -> verifyParse [CSTI 3; CSTI 4; SWAP ] []) |> ignore

    [<TestMethod>]
    member this.ParseExpLdi () =
        verifyParse [CSTI 1; LDI ] [Ldi(Csti 1)]     

    [<TestMethod>]
    member this.ParseExpSti () =
        verifyParse [CSTI 3; CSTI 4; STI ] [Sti(Csti 3, Csti 4)]     

    [<TestMethod>]
    member this.ParseExpGetbp () =
        verifyParse [GETBP ] [Getbp] 

    [<TestMethod>]
    member this.ParseExpGetsp () =
        verifyParse [GETSP ] [Getsp]       

    [<TestMethod>]
    member this.ParseExpGetIncsp () =
        verifyParse [INCSP 1] [Incsp 1]         

    [<TestMethod>]
    member this.ParseExpGetGoto () =
        verifyParse [GOTO "12"] [Goto "12"]   

    [<TestMethod>]
    member this.ParseExpGetIfzero () =
        verifyParse [CSTI 1; IFZERO "12"] [Ifzero("12", Csti 1)] 

    [<TestMethod>]
    member this.ParseExpGetIfnzro () =
        verifyParse [CSTI 1; IFNZRO "12"] [Ifnzro("12", Csti 1)]          

    [<TestMethod>]
    member this.ParseExpGetCall () =
        verifyParse [CSTI 1; CSTI 2; CSTI 3; CALL(3, "12")] [Call(3, "12", [Csti 1; Csti 2; Csti 3])]    

    [<TestMethod>]
    member this.ParseExpGetTcall () =
        verifyParse [CSTI 1; CSTI 2; CSTI 3; TCALL(3, 2, "12")] [Tcall(3, 2, "12", [Csti 1; Csti 2; Csti 3])]     

    [<TestMethod>]
    member this.ParseExpGetRet () =
        verifyParse [CSTI 1; RET 3] [Ret(3, Csti 1)]      

    [<TestMethod>]
    member this.ParseExpGetPrinti () =
        verifyParse [CSTI 1; PRINTI] [Printi(Csti 1)]  

    [<TestMethod>]
    member this.ParseExpGetPrintc () =
        verifyParse [CSTI 1; PRINTC] [Printc(Csti 1)]  

    [<TestMethod>]
    member this.ParseExpGetLdargs () =
        verifyParse [LDARGS] [Ldargs]      

    [<TestMethod>]
    member this.ParseExpGetStop () =
        verifyParse [STOP] [Stop]           

    [<TestMethod>]
    member this.ParseExpGetLabel () =
        verifyParse [Label "12"] [Lab "12"]          