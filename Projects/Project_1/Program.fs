open System

type Tree<'x> = Node of 'x * Tree<'x> list;;

let movetree (Node((label, x), subtrees), dx:float) = Node((label, x + dx), subtrees);;

type Extent = (float * float) list;;

let moveextent (e:Extent, x) = List.map(fun (p, q) -> (p + x, q + x)) e

let rec merge(ps, qs) = match (ps, qs) with
                        | ([], qs) -> qs
                        | (ps, []) -> ps
                        | ((p, _)::ps, (_, q)::qs) -> (p, q)::merge(ps, qs);;

let mergelist es = List.fold merge [] es

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
