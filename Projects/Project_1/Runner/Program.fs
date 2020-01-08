open System
open TreeDesign
open PostScriptGen
open ASTConverter

let ex6 = P([
                VarDec(ITyp, "x")
            ], [
                Ass(AVar("x"), N(3));
                Alt(GC([
                    (Apply("=", [
                        Access(AVar("x")); 
                        N(0)
                    ]), [
                        PrintLn(Access(AVar("x")))
                    ]);
                    (Apply("=", [
                        Access(AVar("x")); 
                        N(0)
                    ]), [
                        PrintLn(N(10))
                    ])
                ]));
                PrintLn(Access(AVar("x")))
            ])

let leftStaircase = Node("Test", [
    Node("Test", [
        Node("Test", [
            Node("Test", [
                Node("Test", [
                    Node("Test", [
                        Node("Test", [
                            Node("Test", []);
                            Node("Test", [])
                        ]);
                        Node("Test", [])
                    ]);
                    Node("Test", [])
                ]);
                Node("Test", [])
            ]);
            Node("Test", [])
        ]);
        Node("Test", [])
    ]);
    Node("Test", [])
])

let rightStaircase = Node("Test", [
    Node("Test", []);
    Node("Test", [    
        Node("Test", []);
        Node("Test", [    
            Node("Test", []);
            Node("Test", [
                Node("Test", []);
                Node("Test", [
                    Node("Test", []);
                    Node("Test", [
                        Node("Test", []);
                        Node("Test", [
                            Node("Test", []);
                            Node("Test", [])
                        ])
                    ])
                ])
            ])
        ])
    ])
])

let rec randomTree n (rnd:Random) = match n with
                                         | 0 -> Node("Test", [])
                                         | _ -> let subTreeCount = rnd.Next(1, Math.Min(10, n + 1))
                                                let subtrees = List.fold(fun (left, nodes) _ -> let allocatedNodes = rnd.Next(left)
                                                                                                (left - allocatedNodes, (randomTree allocatedNodes rnd)::nodes)) (n - subTreeCount, []) [1..(subTreeCount - 1)]
                                                Node("Test", (randomTree (fst subtrees) rnd)::(snd subtrees))
                                       
let makeRandomTree n = randomTree (n - 1) (new Random(34234))



[<EntryPoint>]
let main argv =
    //let result = PostScriptGen.generate (TreeDesign.design randomTree)
    //let result = PostScriptGen.generate (TreeDesign.design (makeRandomTree 50))
    //let result = PostScriptGen.generate (TreeDesign.design leftStaircase)
    let result = PostScriptGen.generate (TreeDesign.design rightStaircase)
    PostScriptGen.psToPdfFile result
    0 // return an integer exit code
