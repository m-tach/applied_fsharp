open System
open TreeDesign
open PostScriptGen
open ASTConverter
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running

let rec randomTree n (rnd:Random) = match n with
                                    | 0 -> Node("Testing" + rnd.Next(0, 100).ToString(), [])
                                    | _ -> let subTreeCount = rnd.Next(0, Math.Min(n, 10))
                                           Node("Testing" + rnd.Next(0, 100).ToString(), List.map(fun _ -> randomTree (n - 1) rnd) [1..subTreeCount])


type BenchmarkPostscriptGen () =
    let mutable tree : Tree<string * float> = Node(("", 0.0), [])

    [<Params (10)>] 
    member val public TreeSize = 0 with get, set

    [<GlobalSetup>]
    member self.GlobalSetupData() =
        tree <- TreeDesign.design (randomTree self.TreeSize (new Random()))

    [<Benchmark>]
    member self.StringConcat () = PostScriptGen.generateConcat tree

    [<Benchmark>]
    member self.StringBuilder () = PostScriptGen.generateBuilder tree

    [<Benchmark>]
    member self.Sequence () = PostScriptGen.generate tree

let defaultSwitch () = BenchmarkSwitcher [|typeof<BenchmarkPostscriptGen>|]

[<EntryPoint>]
let Main args =
    (*let result = PostScriptGen.generate tree
    PostScriptGen.psToPdfFile result*)
    let summary = defaultSwitch().Run args
    0
