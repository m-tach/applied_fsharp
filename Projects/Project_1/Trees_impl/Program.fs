open System

type Tree<'x> = Node of 'x * Tree<'x> list;;

let movetree (Node((label, x), subtrees), dx:float) = Node((label, x + dx), subtrees);;

type Extent = (float * float) list;;

let moveextent (e:Extent, x) = List.map(fun (p, q) -> (p + x, q + x)) e

let rec merge(ps, qs) = match (ps, qs) with
                        | ([], qs) -> qs
                        | (ps, []) -> ps
                        | ((p, _)::ps, (_, q)::qs) -> (p, q)::merge(ps, qs);;

let mergelist es = List.fold(fun x y -> merge(x, y)) [] es;;

let rec fit aa bb = match (aa, bb) with
                    | ((_, p)::ps, (q, _)::qs) -> Math.Max(fit ps qs, p - q + 1.0)
                    | (_, _) -> 0.0;;

let rec fitlistl es = let rec fitlistll acc bb = match bb with
                                                 | [] -> []
                                                 | (b::btail)  -> let x = fit acc b
                                                                  x::(fitlistll (merge (acc, moveextent (b, x))) btail)
                      fitlistll [] es;;

let fitlistr es = let rec fitlistrr acc bb = match bb with
                                             | [] -> []
                                             | (b::btail) -> let x = -(fit b acc)
                                                             x::(fitlistrr (merge (moveextent (b, x), acc)) btail)
                  List.rev (fitlistrr [] (List.rev es));;

let mean (x,y) = (x + y) / 2.0;;
let fitlist es = List.map mean (List.zip (fitlistl es) (fitlistr es));;

let design tree = let rec designn (Node(label, subtrees)) = let (trees, extents) = List.unzip (List.map designn subtrees)
                                                            let positions = fitlist extents
                                                            let ptrees = List.map movetree (List.zip trees positions)
                                                            let pextents = List.map moveextent (List.zip extents positions)
                                                            let resultextent = (0.0, 0.0) :: mergelist pextents
                                                            let resulttree = Node((label, 0.0), ptrees)
                                                            (resulttree, resultextent)
                  fst (designn tree);;


[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
