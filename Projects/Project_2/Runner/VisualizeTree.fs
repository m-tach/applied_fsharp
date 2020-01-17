namespace Visualizer

open System
open System.IO
open System.Diagnostics
open GuardedCommands.Frontend.AST

    type Tree<'x> = Node of 'x * Tree<'x> list

module TreeDesign =

    let private movetree (Node((label, x), subtrees), dx:float) = Node((label, x + dx), subtrees)

    type private Extent = (float * float) list

    let private moveextent (e:Extent, x) = List.map(fun (p, q) -> (p + x, q + x)) e

    let rec private merge(ps, qs) = match (ps, qs) with
                                    | ([], qs) -> qs
                                    | (ps, []) -> ps
                                    | ((p, _)::ps, (_, q)::qs) -> (p, q)::merge(ps, qs)

    let private mergelist es = List.fold(fun x y -> merge(x, y)) [] es

    let rec private fit aa bb = match (aa, bb) with
                                | ((_, p)::ps, (q, _)::qs) -> Math.Max(fit ps qs, p - q + 1.0)
                                | (_, _) -> 0.0

    let rec private fitlistl es = let rec fitlistll acc bb = match bb with
                                                             | [] -> []
                                                             | (b::btail)  -> let x = fit acc b
                                                                              x::(fitlistll (merge (acc, moveextent (b, x))) btail)
                                  fitlistll [] es

    let private fitlistr es = let rec fitlistrr acc bb = match bb with
                                                         | [] -> []
                                                         | (b::btail) -> let x = -(fit b acc)
                                                                         x::(fitlistrr (merge (moveextent (b, x), acc)) btail)
                              List.rev (fitlistrr [] (List.rev es))

    let private mean (x,y) = (x + y) / 2.0
    let private fitlist es = List.map mean (List.zip (fitlistl es) (fitlistr es))

    let public design tree = let rec designn (Node(label, subtrees)) = let (trees, extents) = List.unzip (List.map designn subtrees)
                                                                       let positions = fitlist extents
                                                                       let ptrees = List.map movetree (List.zip trees positions)
                                                                       let pextents = List.map moveextent (List.zip extents positions)
                                                                       let resultextent = (0.0, 0.0) :: mergelist pextents
                                                                       let resulttree = Node((label, 0.0), ptrees)
                                                                       (resulttree, resultextent)
                             fst (designn tree)

module ASTConverter =

    let rec public programToTree (P(decs, stms)) = Node("Program", [decsToNodes decs; stmsToNodes stms])

    and private decsToNodes decs = match decs with
                                   | [dec] -> decToNode dec
                                   | _ -> Node("Declarations", List.map decToNode decs)
    and private decToNode dec = match dec with
                                | VarDec(t, s) -> Node("Variable declaration", [typToNode(t); Node(s, [])])
                                | FunDec(Some(t), s, decs, stm) -> Node("Function  declaration", [typToNode t; Node(s, []); decsToNodes decs; stmToNode stm])
                                | FunDec(None, s, decs, stm) -> Node("Function  declaration", [Node(s, []); decsToNodes decs; stmToNode stm])

    and private typToNode typ = match typ with 
                                | ITyp -> Node("int", [])
                                | BTyp -> Node("bool", [])
                                | CTyp -> Node("char", [])
                                | ATyp(t, Some(x)) -> Node("Array", [typToNode(t); Node(String.Format("Size: {0}", x), [])])
                                | ATyp(t, None) -> Node("Array", [typToNode(t); Node(String.Format("Size: {0}", "Unknown"), [])])
                                | PTyp(t) -> Node("Pointer", [typToNode t])
                                //don't understand what types and t are in this context. Do they define a function call?
                                | FTyp(types, Some(t)) -> Node("Function", (List.map typToNode types) @ [typToNode t])
                                | FTyp(types, None) -> Node("Function", (List.map typToNode types))

    and private stmsToNodes stms = match stms with
                                   | [stm] -> stmToNode stm
                                   | _ -> Node("Statements", List.map stmToNode stms)
    and private stmToNode stm = match stm with
                                | PrintLn(e) -> Node("PrintLn",[expToNode e])
                                | Ass(a, e) -> Node("Assignment", [accToNode a; expToNode e])
                                | MAss(a, e) -> Node("Multiple Assignment", List.map2 (fun ca ce -> stmToNode (Ass(ca, ce))) a e)
                                | Return(Some(e)) -> Node("Return", [expToNode e])
                                | Return(None) -> Node("Return", [])
                                | Alt(gc) -> Node("Alt", guardToNodeList gc)
                                | Do(gc) -> Node("Do", guardToNodeList gc)
                                | Block(decs, stms) -> Node("Block", [decsToNodes decs; stmsToNodes stms])
                                | Call(s, [])  -> Node("Function call", [Node(s, []);])
                                | Call(s, [e]) -> Node("Function call", [Node(s, []); Node("Argument", [expToNode e])])
                                | Call(s, e)   -> Node("Function call", [Node(s, []); Node("Arguments", List.map expToNode e)])

    and private expToNode exp = match exp with
                                | N(n) -> Node(n.ToString(), [])
                                | B(b) -> Node(b.ToString(), [])
                                | STR(s) -> Node("\"" + s.ToString() + "\"", [])
                                | Access(AVar(acc)) -> accToNode (AVar(acc))
                                | Access(a) -> Node("Access", [accToNode a])
                                | Addr(a) -> Node("Address", [accToNode a])
                                | Apply(s, [])  -> Node("Operator", [Node(s, []);])
                                | Apply(s, [e]) -> Node("Operator", [Node(s, []); expToNode e])
                                | Apply(s, [e1; e2])   -> Node("Operator", [expToNode e1; Node(s, []); expToNode e2])
                                | Apply(s, e)   -> Node("Operator", [Node(s, []); Node("Arguments", List.map expToNode e)])

    and private accToNode acc = match acc with
                                | AVar(s) -> Node(s, [])
                                | AIndex(a, e) -> Node("Index", [accToNode a; expToNode e])
                                | ADeref(e) -> Node("Deref", [expToNode e])

    and private guardToNodeList (GC(stuff)) = List.map(fun (x, y) -> Node("guarded", [expToNode x; stmsToNodes y])) stuff

module PostScriptGen =

    // Configs
    [<Literal>]
    let private MOVE_Y_START       = 8.0
    [<Literal>]
    let private MOVE_Y_MIDDLE      = 25.0
    [<Literal>]
    let private MOVE_Y_BOTTOM      = 40.0
    [<Literal>]
    let private MOVE_Y_STEPSIZE    = 50.0
    [<Literal>]
    let private PIXELS_PER_CHARACTER = 7.0

    (*
        Shows what the constant values represent by showing how the
        lines from a parent to child are drawn and what the y values are.
        All values are relative to the y value of the parent label.

                  Parent Label
                        |      <-MOVE_Y_START
                        |
                        |
                        |
                        |
                        |
      MOVE_Y_MIDDLE ->  ---------------------
                                            |
                                            |
                                            |
                                            |
                                            |
                                            |      <- MOVE_Y_BOTTOM
                                       Child Label <- MOVE_Y_STEPSIZE
    *)

    // Finds the width of the tree, used for fitting.
    let rec private findLargestX (Node((_, x), subtree)) : float =
        match subtree with
        | [] -> x
        | _ -> x + List.max (List.map findLargestX subtree)
    
    let rec private findSmallestX (Node((_, x), subtree)) : float =
        match subtree with
        | [] -> x
        | _ -> x + List.min (List.map findSmallestX subtree)

    let rec private findLongestLabel (Node((label, x), subtree)) : int =
        match subtree with
        | [] -> String.length label
        | _ -> List.max ( (String.length label) :: (List.map findLongestLabel subtree) )

    let rec private findTreeDepth tree : float = 
        match tree with
        | Node(_, []) -> 1.0
        | Node(_, subtrees) -> 1.0 + (List.max (List.map findTreeDepth subtrees))
    
    let private getLabelSize tree : float =
        PIXELS_PER_CHARACTER * (float(findLongestLabel tree))

    let private getTreeWidth tree : float =
        (Math.Abs (findLargestX tree) + Math.Abs (findSmallestX tree) + 1.0) * (getLabelSize tree)

    let private getTreeHeight tree : float = 
        ((findTreeDepth tree)) * MOVE_Y_STEPSIZE

    let private getRootX tree : float =
        ( Math.Abs(findSmallestX tree) * (getLabelSize tree) + (getLabelSize tree) / 2.0)

    let private getRootY tree : float =
        (getTreeHeight tree) - (MOVE_Y_STEPSIZE / 2.0)

    // Used to convert floats to pixel values for the PostScript.
    let private toIntString (x : float) =
        string(int(x))
    
    //// Sequences approach - Default for backwards compability with Runner ////

    // The recursive implementation that reads the node, prints its label, scans children nodes and draw lines to those nodes.
    let rec private generateImpl (Node((label, _), subtree)) (x : float) (y : float) (maxX : float) : seq<string> =
        seq {
            yield toIntString x
            yield " "
            yield toIntString y
            yield " moveto\n("
            yield string(label)
            yield ") dup stringwidth pop 2 div neg 0 rmoveto show"
            yield! Seq.concat (List.map (fun (Node((label2, dx), subtree2)) ->
                seq {
                    yield "\nnewpath\n"
                    yield (toIntString x)
                    yield " "
                    yield (toIntString (y - MOVE_Y_START))
                    yield " moveto\n"

                    yield (toIntString x)
                    yield " "
                    yield (toIntString (y - MOVE_Y_MIDDLE))
                    yield " lineto\n"

                    yield (toIntString (x + dx * maxX))
                    yield " "
                    yield (toIntString (y - MOVE_Y_MIDDLE))
                    yield " lineto\n"

                    yield (toIntString (x + dx * maxX))
                    yield " "
                    yield (toIntString (y - MOVE_Y_BOTTOM))
                    yield " lineto\nstroke\n"

                    yield! generateImpl (Node((label2, dx), subtree2)) (x + dx * maxX) (y - MOVE_Y_STEPSIZE) maxX
                }
            ) subtree)
        }

    // The main entry for converting a design-parsed tree into PostScript commands.
    let public generate (designResult : Tree<string*float>) : string =
        let pagewidth = getTreeWidth designResult
        let pageheight = getTreeHeight designResult
        let treeRootX = getRootX designResult
        let treeRootY = getRootY designResult
        String.concat "" (Seq.toList (seq {
            yield "%!\n<</PageSize["
            yield (toIntString (pagewidth))
            yield " "
            yield (toIntString (pageheight))
            yield "]/ImagingBBox null>> setpagedevice\n/Times-Roman findfont 10 scalefont setfont\n1 1 scale\n"
            yield! (generateImpl designResult treeRootX treeRootY (getLabelSize designResult))
            yield "\nshowpage"
        }))

    // Additional function to save the PostScript to a file and open external library GhostScript to view the visual tree.
    // TYPE 'quit' TWICE TO EXIT!
    let public psToPdfFile (psContents : string) =
        File.WriteAllText ("tmp_file.ps", psContents) |> ignore
        Process.Start ("../../../ghostscript/gswin64c.exe", "tmp_file.ps") |> ignore

module Visualizer =
    let public Visualize prog =
        let ast = ASTConverter.programToTree prog
        let tree = TreeDesign.design ast
        let result = PostScriptGen.generate tree
        PostScriptGen.psToPdfFile result