namespace PostScriptGen

open TreeDesign
open System
open System.IO
open System.Diagnostics
open System.Text

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
        7.0 * (float(findLongestLabel tree))

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

    //// StringBuilder approach ////
    
    let private (++) (left : StringBuilder) (right : 't) : StringBuilder =
        left.Append right

    // The recursive implementation that reads the node, prints its label, scans children nodes and draw lines to those nodes.
    let rec private generateImplBuilder (sb : StringBuilder) (Node((label, _), subtree)) (x : float) (y : float) (maxX : float) =
        sb
        ++ (int x) ++ " " ++ (int y) ++ " moveto\n(" ++ label ++ ") dup stringwidth pop 2 div neg 0 rmoveto show" |> ignore

        List.iter (fun (Node((label2, dx), subtree2)) ->
            sb
            ++ "\nnewpath\n"
            ++ (int x) ++ " " ++ (int (y - MOVE_Y_START)) ++ " moveto\n"
            ++ (int x) ++ " " ++ (int (y - MOVE_Y_MIDDLE)) ++ " lineto\n"
            ++ (int (x + dx * maxX)) ++ " " ++ (int (y - MOVE_Y_MIDDLE)) ++ " lineto\n"
            ++ (int (x + dx * maxX)) ++ " " ++ (int (y - MOVE_Y_BOTTOM)) ++ " lineto\nstroke\n" |> ignore
            generateImplBuilder sb (Node((label2, dx), subtree2)) (x + dx * maxX) (y - MOVE_Y_STEPSIZE) maxX
        ) subtree

    // The main entry for converting a design-parsed tree into PostScript commands.
    let public generateBuilder (designResult : Tree<string*float>) : string =
        let pagewidth = getTreeWidth designResult
        let pageheight = getTreeHeight designResult
        let treeRootX = getRootX designResult
        let treeRootY = getRootY designResult
        let s = StringBuilder()
        s.Append "%!\n<</PageSize[" |> ignore
        s.Append (toIntString pagewidth) |> ignore
        s.Append " " |> ignore
        s.Append (toIntString pageheight) |> ignore
        s.Append "]/ImagingBBox null>> setpagedevice\n/Times-Roman findfont 10 scalefont setfont\n1 1 scale\n" |> ignore
        generateImplBuilder s designResult treeRootX treeRootY (getLabelSize designResult)
        s.Append "\nshowpage" |> ignore
        s.ToString()
    
    //// String.concat approach ////
    
    // The recursive implementation that reads the node, prints its label, scans children nodes and draw lines to those nodes.
    let rec private generateImplConcat (Node((label, _), subtree)) (x : float) (y : float) (maxX : float) : string =
        String.concat "" [
            toIntString(x);
            " ";
            toIntString(y);
            " moveto\n(";
            label;
            ") dup stringwidth pop 2 div neg 0 rmoveto show";
            (String.concat "\n" (List.map (fun (Node((label2, dx), subtree2)) -> String.concat "" [
                "\nnewpath\n";

                toIntString(x);
                " ";
                toIntString(y - MOVE_Y_START);
                " moveto\n";
                
                toIntString(x);
                " ";
                toIntString(y - MOVE_Y_MIDDLE);
                " lineto\n";
                
                toIntString(x + dx * maxX);
                " ";
                toIntString(y - MOVE_Y_MIDDLE);
                " lineto\n";

                toIntString(x + dx * maxX);
                " ";
                toIntString(y - MOVE_Y_BOTTOM);
                " lineto\nstroke\n";
                (generateImplConcat (Node((label2, dx), subtree2)) (x + dx * maxX) (y - MOVE_Y_STEPSIZE) maxX)
            ]) subtree))
        ]

    // The main entry for converting a design-parsed tree into PostScript commands.
    let public generateConcat (designResult : Tree<string*float>) : string =
        let pagewidth = getTreeWidth designResult
        let pageheight = getTreeHeight designResult
        let treeRootX = getRootX designResult
        let treeRootY = getRootY designResult
        String.concat "" [
            "%!\n<</PageSize["
            (toIntString pagewidth)
            " "
            (toIntString pageheight)
            "]/ImagingBBox null>> setpagedevice\n/Times-Roman findfont 10 scalefont setfont\n1 1 scale\n";
            (generateImplConcat designResult treeRootX treeRootY (getLabelSize designResult));
            "\nshowpage"
        ]

    //// String.concat approach v2 - Single String.concat, but lots of lists... ////
    
    // The recursive implementation that reads the node, prints its label, scans children nodes and draw lines to those nodes.
    let rec private generateImplConcat2 (Node((label, _), subtree)) (x : float) (y : float) (maxX : float) : List<string> =
        [toIntString(x); " "; toIntString(y); " moveto\n("; label; ") dup stringwidth pop 2 div neg 0 rmoveto show"] @
        Seq.toList (Seq.concat (List.map (fun (Node((label2, dx), subtree2)) -> ([
                                                            "\nnewpath\n";
                                                            toIntString(x); " "; toIntString(y - MOVE_Y_START); " moveto\n";
                                                            toIntString(x); " "; toIntString(y - MOVE_Y_MIDDLE); " lineto\n";
                                                            toIntString(x + dx * maxX); " "; toIntString(y - MOVE_Y_MIDDLE); " lineto\n";
                                                            toIntString(x + dx * maxX); " "; toIntString(y - MOVE_Y_BOTTOM); " lineto\nstroke\n";
                                                        ] @ (generateImplConcat2 (Node((label2, dx), subtree2)) (x + dx * maxX) (y - MOVE_Y_STEPSIZE) maxX))) subtree))

    // The main entry for converting a design-parsed tree into PostScript commands.
    let public generateConcat2 (designResult : Tree<string*float>) : string =
        let pagewidth = getTreeWidth designResult
        let pageheight = getTreeHeight designResult
        let treeRootX = getRootX designResult
        let treeRootY = getRootY designResult
        String.concat "" (["%!\n<</PageSize["; (toIntString pagewidth); " "; (toIntString pageheight);
        "]/ImagingBBox null>> setpagedevice\n/Times-Roman findfont 10 scalefont setfont\n1 1 scale\n"] @
        (generateImplConcat2 designResult treeRootX treeRootY (getLabelSize designResult)) @ ["\nshowpage"])

    // Additional function to save the PostScript to a file and open external library GhostScript to view the visual tree.
    // TYPE 'quit' TWICE TO EXIT!
    let public psToPdfFile (psContents : string) =
        File.WriteAllText ("tmp_file.ps", psContents) |> ignore
        Process.Start ("../../../ghostscript/gswin64c.exe", "tmp_file.ps") |> ignore
