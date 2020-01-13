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
type TestParse () =

    [<TestMethod>]
    member this.ParseA0 () =
        parseFromFile "programs/A0.gc" |> ignore

    [<TestMethod>]
    member this.ParseA1 () =
        parseFromFile "programs/A1.gc" |> ignore

    [<TestMethod>]
    member this.ParseA2 () =
        parseFromFile "programs/A2.gc" |> ignore

    [<TestMethod>]
    member this.ParseA3 () =
        parseFromFile "programs/A3.gc" |> ignore

    [<TestMethod>]
    member this.ParseA4 () =
        parseFromFile "programs/A4.gc" |> ignore

    [<TestMethod>]
    member this.ParseEx0 () =
        parseFromFile "programs/Ex0.gc" |> ignore

    [<TestMethod>]
    member this.ParseEx1 () =
        parseFromFile "programs/Ex1.gc" |> ignore

    [<TestMethod>]
    member this.ParseEx2 () =
        parseFromFile "programs/Ex2.gc" |> ignore

    [<TestMethod>]
    member this.ParseEx3 () =
        parseFromFile "programs/Ex3.gc" |> ignore

    [<TestMethod>]
    member this.ParseEx4 () =
        parseFromFile "programs/Ex4.gc" |> ignore

    [<TestMethod>]
    member this.ParseEx5 () =
        parseFromFile "programs/Ex5.gc" |> ignore

    [<TestMethod>]
    member this.ParseEx6 () =
        parseFromFile "programs/Ex6.gc" |> ignore

    [<TestMethod>]
    member this.ParseEx7 () =
        parseFromFile "programs/Ex7.gc" |> ignore

    [<TestMethod>]
    member this.ParseExa1 () =
        parseFromFile "programs/Exa1.gc" |> ignore    

    [<TestMethod>]
    member this.ParseFact () =
        parseFromFile "programs/fact.gc" |> ignore

    [<TestMethod>]
    member this.ParseFactRec () =
        parseFromFile "programs/factRec.gc" |> ignore

    [<TestMethod>]
    member this.ParseFactCBV () =
        parseFromFile "programs/factCBV.gc" |> ignore

    [<TestMethod>]
    member this.ParseFactImPTyp () =
        parseFromFile "programs/factImPTyp.gc" |> ignore   

    [<TestMethod>]
    member this.ParseGCDo () =
        parseFromFile "programs/GC_Do.gc" |> ignore      

    [<TestMethod>]
    member this.ParseProcedure1 () =
        parseFromFile "programs/basicProc1.gc" |> ignore

    [<TestMethod>]
    member this.ParseProcedure2 () =
        parseFromFile "programs/basicProc2.gc" |> ignore    

    [<TestMethod>]
    member this.ParseProcedure3 () =
        parseFromFile "programs/basicProc3.gc" |> ignore

    [<TestMethod>]
    member this.ParseProcedure4 () =
        parseFromFile "programs/basicProc4.gc" |> ignore     

    [<TestMethod>]
    member this.Parsepar1 () =
        parseFromFile "programs/par1.gc" |> ignore  

    [<TestMethod>]
    member this.Parsepar2 () =
        parseFromFile "programs/par2.gc" |> ignore    

    [<TestMethod>]
    member this.Parsepartition () =
        parseFromFile "programs/partition.gc" |> ignore

    [<TestMethod>]
    member this.ParseQuickSortV1 () =
        parseFromFile "programs/QuickSortV1.gc" |> ignore

    [<TestMethod>]
    member this.ParseQuickSortV2 () =
        parseFromFile "programs/QuickSortV2.gc" |> ignore    

    [<TestMethod>]
    member this.ParseSkip () =
        parseFromFile "programs/skip.gc" |> ignore  

    [<TestMethod>]
    member this.ParseSwap () =
        parseFromFile "programs/swap.gc" |> ignore
