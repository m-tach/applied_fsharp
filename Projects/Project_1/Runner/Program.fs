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

let randomTree = Node("hello", [
    Node("world", [
        Node("abc", []);
        Node("def", [])]
    );
    Node("f#", [
        Node("hello", [
            Node("world", [
                Node("abc", []);
                Node("def", [
                    Node("hello", [
                        Node("world", [
                            Node("abc", []);
                            Node("def", [])]
                        );
                        Node("f#", [])
                    ])
                ]);Node("def", [
                    Node("hello", [
                        Node("world", [
                            Node("abc", []);
                            Node("def", [])]
                        );
                        Node("f#", [])
                    ])
                ]);Node("def", [
                    Node("hello", [
                        Node("world", [
                            Node("abc", []);
                            Node("def", [])]
                        );
                        Node("f#", [])
                    ])
                ]);Node("def", [
                    Node("hello", [
                        Node("world", [
                            Node("abc", []);
                            Node("def", [])]
                        );
                        Node("f#", [])
                    ])
                ])]
            );
            Node("f#", [])
        ])
    ])
])

[<EntryPoint>]
let main argv =
    //let result = PostScriptGen.generate (TreeDesign.design randomTree)
    let result = PostScriptGen.generate (TreeDesign.design (ASTConverter.programToTree ex6))
    PostScriptGen.psToPdfFile result
    0 // return an integer exit code
