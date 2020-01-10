namespace Tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

open GuardedCommands.Util
open GuardedCommands.Frontend.TypeCheck
open GuardedCommands.Frontend.AST
open GuardedCommands.Backend.CodeGeneration

open ParserUtil
open CompilerUtil

module Assert =
    let Throws<'a> message f =
        let mutable wasThrown = false
        try
            f()
        with ex ->
            Assert.AreEqual(ex.GetType(), typedefof<'a>, (sprintf "Actual Exception: %A" ex))
            Assert.AreEqual(ex.Message, message, (sprintf "Exception message: %A" ex.Message))
            wasThrown <- true

        Assert.IsTrue(wasThrown, "No exception thrown")

// Testing typechecking in ILL-typed programs;
/// Each test method verifies a type of <cref="AST.Exp"> as defined in "Parser.fsy"
[<TestClass>]
type IllTypedTests() =
    let missingDeclaration = "begin
    res := 4;
    print res + 2
    end"

    let illegalPLUS = "begin
    res : bool;
    res := true;
    print res + 2
    end"

    let illegalPLUS2 = "begin
    res : int;
    res := 2;
    print res + true
    end"

    let illegalAND = "begin
    res : bool;
    res := true;
    print res && 2
    end"

    let illegalEQ = "begin
    res : bool;
    res := true;
    print res = 2
    end"

    let illegalNEQ = "begin
    res : bool;
    res := true;
    print res <> 2
    end"

    let illegalLT = "begin
    res : bool;
    res := true;
    print res < 2
    end"

    let illegalGT = "begin
    res : bool;
    res := true;
    print res > 2
    end"

    let illegalLE = "begin
    res : bool;
    res := true;
    print res <= 2
    end"

    let illegalMonadicMINUS = "begin
    res : bool;
    res := true;
    print - res
    end"

    let illegalDiadicMINUS = "begin
    res : bool;
    res := true;
    print res - 2
    end"

    let illegalTIMES = "begin
    res : bool;
    res := true;
    print res * 2
    end"

    let illegalNEG = "begin
    res : int;
    res := 2;
    print ! res
    end"

    let illegalIF = "begin 
    x: int; 
    x:= 3; 
    if x -> print x
    | x=0 -> print 10
    fi;
    print x
    end"

    [<TestMethod>]
    member this.VariableDeclarationException() =
        Assert.Throws<Exception> "no declaration for : res"  (fun () -> tcP (parseString missingDeclaration))

    [<TestMethod>]
    member this.IllegalNEGException() = 
        Assert.Throws<Exception> "illegal/illtyped monadic expression: !" (fun () -> tcP (parseString illegalNEG))

    [<TestMethod>]
    member this.IllegalTIMESException() = 
        Assert.Throws<Exception> "illegal/illtyped dyadic expression: *" (fun () -> tcP (parseString illegalTIMES))


    [<TestMethod>]
    member this.IllegalPLUSException() = 
        Assert.Throws<Exception> "illegal/illtyped dyadic expression: +" (fun () -> tcP (parseString illegalPLUS))

    [<TestMethod>]
    member this.IllegalPlus2Exception() = 
        Assert.Throws<Exception> "illegal/illtyped dyadic expression: +" (fun () -> tcP (parseString illegalPLUS2))

    [<TestMethod>]
    member this.IllegalDiadicMINUSException() = 
        Assert.Throws<Exception> "illegal/illtyped dyadic expression: -" (fun () -> tcP (parseString illegalDiadicMINUS))

    [<TestMethod>]
    member this.IllegalMonadicMINUSException() = 
        Assert.Throws<Exception> "illegal/illtyped monadic expression: -" (fun () -> tcP (parseString illegalMonadicMINUS))


    [<TestMethod>]
    member this.IllegalANDException() = 
        Assert.Throws<Exception> "illegal/illtyped dyadic expression: &&" (fun () -> tcP (parseString illegalAND))

    [<TestMethod>]
    member this.IllegalEQException() = 
        Assert.Throws<Exception> "illegal/illtyped dyadic expression: =" (fun () -> tcP (parseString illegalEQ))

    [<TestMethod>]
    member this.IllegalLException() = 
        Assert.Throws<Exception> "illegal/illtyped dyadic expression: <=" (fun () -> tcP (parseString illegalLE))
    
    [<TestMethod>]
    member this.IllegalGTException() = 
        Assert.Throws<Exception> "illegal/illtyped dyadic expression: >" (fun () -> tcP (parseString illegalGT))

    [<TestMethod>]
    member this.IllegalLTException() = 
        Assert.Throws<Exception> "illegal/illtyped dyadic expression: <" (fun () -> tcP (parseString illegalLT))
        
    [<TestMethod>]
    member this.IllegalNEQException() = 
        Assert.Throws<Exception> "illegal/illtyped dyadic expression: <>" (fun () -> tcP (parseString illegalNEQ))

    [<TestMethod>]
    member this.IllegalIFException() = 
        Assert.Throws<Exception> "Illegal use of integer in alternative stm" (fun () -> tcP (parseString illegalIF))





