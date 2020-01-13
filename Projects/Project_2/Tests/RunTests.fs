namespace Tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

open GuardedCommands.Util
open GuardedCommands.Frontend.TypeCheck
open GuardedCommands.Frontend.AST
open GuardedCommands.Backend.CodeGeneration

open ParserUtil
open CompilerUtil

[<TestClass>]
type TestRun () =

    [<TestMethod>]
    member this.RunEx0 () =
        let ast = parseFromFile "programs/Ex0.gc"
        let intrs = CP ast
        let intInstrs = Machine.code2ints intrs
        VirtualMachine.run intInstrs |> ignore

    [<TestMethod>]
    member this.RunEx1 () =
        let ast = parseFromFile "programs/Ex1.gc"
        let intrs = CP ast
        let intInstrs = Machine.code2ints intrs
        VirtualMachine.run intInstrs |> ignore

    [<TestMethod>]
    member this.RunEx2 () =
        let ast = parseFromFile "programs/Ex2.gc"
        let intrs = CP ast
        let intInstrs = Machine.code2ints intrs
        VirtualMachine.run intInstrs |> ignore

    [<TestMethod>]
    member this.RunEx3 () =
        let ast = parseFromFile "programs/Ex3.gc"
        let intrs = CP ast
        let intInstrs = Machine.code2ints intrs
        VirtualMachine.run intInstrs |> ignore

    [<TestMethod>]
    member this.RunEx4 () =
        let ast = parseFromFile "programs/Ex4.gc"
        let intrs = CP ast
        let intInstrs = Machine.code2ints intrs
        VirtualMachine.run intInstrs |> ignore

    [<TestMethod>]
    member this.RunEx5 () =
        let ast = parseFromFile "programs/Ex5.gc"
        let intrs = CP ast
        let intInstrs = Machine.code2ints intrs
        VirtualMachine.run intInstrs |> ignore

    [<TestMethod>]
    member this.RunEx6 () =
        let ast = parseFromFile "programs/Ex6.gc"
        let intrs = CP ast
        let intInstrs = Machine.code2ints intrs
        VirtualMachine.run intInstrs |> ignore

    [<TestMethod>]
    member this.RunEx7 () =
        let ast = parseFromFile "programs/Ex7.gc"
        let intrs = CP ast
        let intInstrs = Machine.code2ints intrs
        VirtualMachine.run intInstrs |> ignore

    [<TestMethod>]
    member this.RunSkip () =
        let ast = parseFromFile "programs/Skip.gc"
        let intrs = CP ast
        let intInstrs = Machine.code2ints intrs
        VirtualMachine.run intInstrs |> ignore

    [<TestMethod>]
    member this.RunFact () =
        let ast = parseFromFile "programs/fact.gc"
        let intrs = CP ast
        let intInstrs = Machine.code2ints intrs
        VirtualMachine.run intInstrs |> ignore

    [<TestMethod>]
    member this.RunFactRec () =
        let ast = parseFromFile "programs/factRec.gc"
        let intrs = CP ast
        let intInstrs = Machine.code2ints intrs
        VirtualMachine.run intInstrs |> ignore

    [<TestMethod>]
    member this.RunFactCBV () =
        let ast = parseFromFile "programs/factCBV.gc"
        let intrs = CP ast
        let intInstrs = Machine.code2ints intrs
        VirtualMachine.run intInstrs |> ignore

    [<TestMethod>]
    member this.RunProcedure1 () =
        let ast = parseFromFile "programs/basicProc1.gc"
        let intrs = CP ast
        let intInstrs = Machine.code2ints intrs
        VirtualMachine.runTrace intInstrs |> ignore

    [<TestMethod>]
    member this.RunProcedure2 () =
        let ast = parseFromFile "programs/basicProc2.gc"
        let intrs = CP ast
        let intInstrs = Machine.code2ints intrs
        VirtualMachine.run intInstrs |> ignore

    [<TestMethod>]
    member this.RunProcedure3 () =
        let ast = parseFromFile "programs/basicProc3.gc"
        let intrs = CP ast
        let intInstrs = Machine.code2ints intrs
        VirtualMachine.run intInstrs |> ignore

    [<TestMethod>]
    member this.RunProcedure4 () =
        let ast = parseFromFile "programs/basicProc4.gc"
        let intrs = CP ast
        let intInstrs = Machine.code2ints intrs
        VirtualMachine.runTrace intInstrs |> ignore        

