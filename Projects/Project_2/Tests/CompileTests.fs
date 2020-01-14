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
type TestCompile () =

    [<TestMethod>]
    member this.CompileA0 () =
        let ast = parseFromFile "programs/A0.gc"
        CP ast |> ignore

    [<TestMethod>]
    member this.CompileA1 () =
        let ast = parseFromFile "programs/A1.gc"
        CP ast |> ignore

    [<TestMethod>]
    member this.CompileA2 () =
        let ast = parseFromFile "programs/A2.gc"
        CP ast |> ignore

    [<TestMethod>]
    member this.CompileA3 () =
        let ast = parseFromFile "programs/A3.gc"
        CP ast |> ignore

    [<TestMethod>]
    member this.CompileA4 () =
        let ast = parseFromFile "programs/A4.gc"
        CP ast |> ignore

    [<TestMethod>]
    member this.CompileEx0 () =
        let ast = parseFromFile "programs/Ex0.gc"
        CP ast |> ignore

    [<TestMethod>]
    member this.CompileEx1 () =
        let ast = parseFromFile "programs/Ex1.gc"
        CP ast |> ignore

    [<TestMethod>]
    member this.CompileEx2 () =
        let ast = parseFromFile "programs/Ex2.gc"
        CP ast |> ignore

    [<TestMethod>]
    member this.CompileEx3 () =
        let ast = parseFromFile "programs/Ex3.gc"
        CP ast |> ignore

    [<TestMethod>]
    member this.CompileEx4 () =
        let ast = parseFromFile "programs/Ex4.gc"
        CP ast |> ignore

    [<TestMethod>]
    member this.CompileEx5 () =
        let ast = parseFromFile "programs/Ex5.gc"
        CP ast |> ignore

    [<TestMethod>]
    member this.CompileEx6 () =
        let ast = parseFromFile "programs/Ex6.gc"
        CP ast |> ignore

    [<TestMethod>]
    member this.CompileEx7 () =
        let ast = parseFromFile "programs/Ex7.gc"
        CP ast |> ignore

    [<TestMethod>]
    member this.CompileExa1 () =
        let ast = parseFromFile "programs/Exa1.gc"
        CP ast |> ignore    

    [<TestMethod>]
    member this.CompileFact () =
        let ast = parseFromFile "programs/fact.gc"
        CP ast |> ignore

    [<TestMethod>]
    member this.CompileFactRec () =
        let ast = parseFromFile "programs/factRec.gc"
        CP ast |> ignore

    [<TestMethod>]
    member this.CompileFactCBV () =
        let ast = parseFromFile "programs/factCBV.gc"
        CP ast |> ignore

    [<TestMethod>]
    member this.CompileFactImPTyp () =
        let ast = parseFromFile "programs/factImpPTyp.gc"
        CP ast |> ignore

    [<TestMethod>]
    member this.CompileGCDo () =
        let ast = parseFromFile "programs/GC_Do.gc"
        CP ast |> ignore        

    [<TestMethod>]
    member this.CompileProcedure1 () =
        let ast = parseFromFile "programs/basicProc1.gc"
        CP ast |> ignore

    [<TestMethod>]
    member this.CompileProcedure2 () =
        let ast = parseFromFile "programs/basicProc2.gc"
        CP ast |> ignore  

    [<TestMethod>]
    member this.CompileProcedure3 () =
        let ast = parseFromFile "programs/basicProc3.gc"
        CP ast |> ignore

    [<TestMethod>]
    member this.CompileProcedure4 () =
        let ast = parseFromFile "programs/basicProc4.gc"
        CP ast |> ignore  

    [<TestMethod>]
    member this.Compilepar1 () =
        let ast = parseFromFile "programs/par1.gc"
        CP ast |> ignore  

    [<TestMethod>]
    member this.Compilepar2 () =
        let ast = parseFromFile "programs/par2.gc"
        CP ast |> ignore   

    [<TestMethod>]
    member this.Compilepartition () =
        let ast = parseFromFile "programs/partition.gc"
        CP ast |> ignore     

    [<TestMethod>]
    member this.CompileQuickSortV1 () =
        let ast = parseFromFile "programs/QuickSortV1.gc"
        CP ast |> ignore  

    [<TestMethod>]
    member this.CompileQuickSortV2 () =
        let ast = parseFromFile "programs/QuickSortV2.gc"
        CP ast |> ignore   

    [<TestMethod>]
    member this.CompileSkip () =
        let ast = parseFromFile "programs/skip.gc"
        CP ast |> ignore  

    [<TestMethod>]
    member this.CompileSwap () =
        let ast = parseFromFile "programs/swap.gc"
        CP ast |> ignore        

    [<TestMethod>]
    member this.CompilePointers1 () =
        let ast = parseFromFile "programs/pointers3.gc"
        CP ast |> ignore

    [<TestMethod>]
    member this.CompilePointers2 () =
        let ast = parseFromFile "programs/pointers4.gc"
        CP ast |> ignore

    [<TestMethod>]
    member this.CompilePointers3 () =
        let ast = parseFromFile "programs/pointers5.gc"
        CP ast |> ignore

    [<TestMethod>]
    member this.CompilePointers4 () =
        let ast = parseFromFile "programs/pointers6.gc"
        CP ast |> ignore                        
