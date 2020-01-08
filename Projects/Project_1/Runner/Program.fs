open System
open TreeDesign
open PostScriptGen
open ASTConverter

let longLabelTree = Node ("Parent", [Node ("_THISISAVERYLONGNAME__THISISAVERYLONGNAME__THISISAVERYLONGNAME__THISISAVERYLONGNAME__THISISAVERYLONGNAME__THISISAVERYLONGNAME__THISISAVERYLONGNAME_",[]); Node ("_THISISAVERYLONGNAME__THISISAVERYLONGNAME__THISISAVERYLONGNAME__THISISAVERYLONGNAME__THISISAVERYLONGNAME__THISISAVERYLONGNAME__THISISAVERYLONGNAME_",[])]);

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

let factTree = P([
                VarDec(ITyp, "res");
                FunDec(Some(ITyp), "fact", [
                        VarDec(ITyp, "n");
                        ],
                    Block([
                            VarDec(ITyp, "x"); //line 4:            x:int, y:int ;
                            VarDec(ITyp, "y"); //line 4:             x:int, y:int ;]))
                        ], [
                            Ass(AVar("x"), Access(AVar("n"))); //line 5:     x:= n ;
                            Ass(AVar("y"), Access(AVar("1"))); //line 6:     y:= 1 ;                        
                            Do(GC([
                                (Apply("!", [
                                    (Apply("=", [
                                        Access(AVar("x")); 
                                        N(0)
                                    ]))]), [
                                            Ass(AVar("y"), Apply("*", [Access(AVar("x")); Access(AVar("y"))] ));    //line 7: y := x*y;
                                            Ass(AVar("x"), Apply("-", [Access(AVar("x")); N(1)] ));    //line 7: x:=x-1                                                            
                                    ])]));
                            Return(Some(Access(AVar("y")))); //line 8:     return y
                        ]                                           
                    ))], [
                        //Statements
                        Ass(AVar("res"), Apply("fact", [N 4]));//line 10:  res := fact(4);
                        PrintLn(Access(AVar("res")))//line 11:            print res   
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
    //let result = PostScriptGen.generate (TreeDesign.design rightStaircase)
    //let result = PostScriptGen.generate (TreeDesign.design rightStaircase)
    //let result = PostScriptGen.generate (TreeDesign.design longLabelTree)

    let result = PostScriptGen.generate (TreeDesign.design (ASTConverter.programToTree factTree))

    PostScriptGen.psToPdfFile result
    0 // return an integer exit code
