open System
open TreeDesign
open PostScriptGen
open ASTConverter
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running

let rec randomTree n (rnd:Random) = match n with
                                         | 0 -> Node("Test", [])
                                         | _ -> let subTreeCount = rnd.Next(1, Math.Min(10, n + 1))
                                                let subtrees = List.fold(fun (left, nodes) _ -> let allocatedNodes = rnd.Next(left)
                                                                                                (left - allocatedNodes, (randomTree allocatedNodes rnd)::nodes)) (n - subTreeCount, []) [1..(subTreeCount - 1)]
                                                Node("Test", (randomTree (fst subtrees) rnd)::(snd subtrees))
                                       
let makeRandomTree n = randomTree (n - 1) (new Random(334234))

type BenchmarkPostscriptGen () =
    let mutable tree : Tree<string * float> = Node(("", 0.0), [])

    [<Params (100, 500, 1000, 5000, 10000)>]
    member val public TreeSize = 0 with get, set

    [<GlobalSetup>]
    member self.GlobalSetupData() =
        tree <- TreeDesign.design (makeRandomTree self.TreeSize)

    [<Benchmark>]
    member self.StringConcat () = PostScriptGen.generateConcat tree

    [<Benchmark>]
    member self.StringConcat2 () = PostScriptGen.generateConcat2 tree

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
