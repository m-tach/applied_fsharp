namespace PostScriptGen

open TreeDesign
open System
open System.IO
open System.Diagnostics
open System.Text

module PostScriptGen =

    // Configs
    let PAGE_WIDTH_SCALING = 200.0
    let ROOT_X_OFFSET      = 70.0
    let ROOT_X_SCALING     = 240.0

    let MOVE_Y_START       = 8.0
    let MOVE_Y_MIDDLE      = 25.0
    let MOVE_Y_BOTTOM      = 40.0
    let MOVE_Y_STEPSIZE    = 50.0

    // Finds the width of the tree, used for fitting.
    let rec findLargestX (Node((_, x), subtree)) : float =
        match subtree with
        | [] -> x
        | _ -> x + List.max (List.map findLargestX subtree)
    
    let rec findSmallestX (Node((_, x), subtree)) : float =
        match subtree with
        | [] -> x
        | _ -> x + List.min (List.map findLargestX subtree)

    let rec findLongestLabel (Node((label, x), subtree)) : int =
        match subtree with
        | [] -> String.length label
        | _ -> List.max ( (String.length label) :: (List.map findLongestLabel subtree) )
    
    let getScalingX tree : float =
        0.17 / float(findLongestLabel tree)

    let findWidth tree : float =
        (Math.Abs (findLargestX tree) + Math.Abs (findSmallestX tree)) * PAGE_WIDTH_SCALING + (getScalingX tree) * 2.0

    let findRootX tree : float =
        (Math.Abs (findSmallestX tree)) * ROOT_X_SCALING + (getScalingX tree) + ROOT_X_OFFSET

    // Used to convert floats to pixel values for the PostScript.
    let toIntString (x : float) =
        string(int(x))
    
    //// Sequences approach - Default for backwards compability with Runner ////

    // The recursive implementation that reads the node, prints its label, scans children nodes and draw lines to those nodes.
    let rec generateImpl (Node((label, _), subtree)) (x : float) (y : float) (maxX : float) : seq<string> =
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
                    yield (toIntString (x + dx / maxX))
                    yield " "
                    yield (toIntString (y - MOVE_Y_MIDDLE))
                    yield " lineto\n"
                    yield (toIntString (x + dx / maxX))
                    yield " "
                    yield (toIntString (y - MOVE_Y_BOTTOM))
                    yield " lineto\nstroke\n"
                    yield! generateImpl (Node((label2, dx), subtree2)) (x + dx / maxX) (y - MOVE_Y_STEPSIZE) maxX
                }
            ) subtree)
        }

    // The main entry for converting a design-parsed tree into PostScript commands.
    let generate (designResult : Tree<string*float>) : string =
        let width = findWidth designResult
        String.concat "" (Seq.toList (seq {
            yield "%!\n<</PageSize["
            yield (toIntString (width + 200.0))
            yield " 1400]/ImagingBBox null>> setpagedevice\n/Times-Roman findfont 10 scalefont setfont\n1 1 scale\n"
            yield! (generateImpl designResult ((findRootX designResult) + 100.0) 1340.0 (getScalingX designResult))
            yield "\nshowpage"
        }))

    //// StringBuilder approach ////
    
    let (++) (left : StringBuilder) (right : 't) : StringBuilder =
        left.Append right

    // The recursive implementation that reads the node, prints its label, scans children nodes and draw lines to those nodes.
    let rec generateImplBuilder (sb : StringBuilder) (Node((label, _), subtree)) (x : float) (y : float) (maxX : float) =
        sb
        ++ (int x) ++ " " ++ (int y) ++ " moveto\n(" ++ label ++ ") dup stringwidth pop 2 div neg 0 rmoveto show" |> ignore

        List.iter (fun (Node((label2, dx), subtree2)) ->
            sb
            ++ "\nnewpath\n"
            ++ (int x) ++ " " ++ (int y - 8) ++ " moveto\n"
            ++ (int x) ++ " " ++ (int y - 25) ++ " lineto\n"
            ++ (int (x + dx / maxX)) ++ " " ++ (int y - 25) ++ " lineto\n"
            ++ (int (x + dx / maxX)) ++ " " ++ (int y - 40) ++ " lineto\nstroke\n" |> ignore
            generateImplBuilder sb (Node((label2, dx), subtree2)) (x + dx / maxX) (y - 50.0) maxX
        ) subtree

    // The main entry for converting a design-parsed tree into PostScript commands.
    let generateBuilder (designResult : Tree<string*float>) : string =
        let width = findWidth designResult
        let s = StringBuilder()
        s.Append "%!\n<</PageSize[" |> ignore
        s.Append (toIntString (width + 200.0)) |> ignore
        s.Append " 1400]/ImagingBBox null>> setpagedevice\n/Times-Roman findfont 10 scalefont setfont\n1 1 scale\n" |> ignore
        generateImplBuilder s designResult ((findRootX designResult) + 100.0) 1340.0 (getScalingX designResult)
        s.Append "\nshowpage" |> ignore
        s.ToString()
    
    //// String.concat approach ////
    
    // The recursive implementation that reads the node, prints its label, scans children nodes and draw lines to those nodes.
    let rec generateImplConcat (Node((label, _), subtree)) (x : float) (y : float) (maxX : float) : string =
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
                toIntString(y - 8.0);
                " moveto\n";
                
                toIntString(x);
                " ";
                toIntString(y - 25.0);
                " lineto\n";
                
                toIntString(x + dx / maxX);
                " ";
                toIntString(y - 25.0);
                " lineto\n";

                toIntString(x + dx / maxX);
                " ";
                toIntString(y - 40.0);
                " lineto\nstroke\n";
                (generateImplConcat (Node((label2, dx), subtree2)) (x + dx / maxX) (y - 50.0) maxX)
            ]) subtree))
        ]

    // The main entry for converting a design-parsed tree into PostScript commands.
    let generateConcat (designResult : Tree<string*float>) : string =
        let width = findWidth designResult
        String.concat "" [
            "%!\n<</PageSize["
            (toIntString (width + 200.0))
            " 1400]/ImagingBBox null>> setpagedevice\n/Times-Roman findfont 10 scalefont setfont\n1 1 scale\n";
            (generateImplConcat designResult ((findRootX designResult) + 100.0) 1340.0 (getScalingX designResult));
            "\nshowpage"
        ]

    //// String.concat approach v2 - Single String.concat, but lots of lists... ////
    
    // The recursive implementation that reads the node, prints its label, scans children nodes and draw lines to those nodes.
    let rec generateImplConcat2 (Node((label, _), subtree)) (x : float) (y : float) (maxX : float) : List<string> =
        [toIntString(x); " "; toIntString(y); " moveto\n("; label; ") dup stringwidth pop 2 div neg 0 rmoveto show"] @
        Seq.toList (Seq.concat (List.map (fun (Node((label2, dx), subtree2)) -> ([
                                                            "\nnewpath\n";
                                                            toIntString(x); " "; toIntString(y - 8.0); " moveto\n";
                                                            toIntString(x); " "; toIntString(y - 25.0); " lineto\n";
                                                            toIntString(x + dx / maxX); " "; toIntString(y - 25.0); " lineto\n";
                                                            toIntString(x + dx / maxX); " "; toIntString(y - 40.0); " lineto\nstroke\n";
                                                        ] @ (generateImplConcat2 (Node((label2, dx), subtree2)) (x + dx / maxX) (y - 50.0) maxX))) subtree))

    // The main entry for converting a design-parsed tree into PostScript commands.
    let generateConcat2 (designResult : Tree<string*float>) : string =
        let width = findWidth designResult
        String.concat "" (["%!\n<</PageSize["; (toIntString (width + 200.0));
        " 1400]/ImagingBBox null>> setpagedevice\n/Times-Roman findfont 10 scalefont setfont\n1 1 scale\n"] @
        (generateImplConcat2 designResult ((findRootX designResult) + 100.0) 1340.0 (getScalingX designResult)) @ ["\nshowpage"])

    // Additional function to save the PostScript to a file and open external library GhostScript to view the visual tree.
    // TYPE 'quit' TWICE TO EXIT!
    let psToPdfFile (psContents : string) =
        File.WriteAllText ("tmp_file.ps", psContents) |> ignore
        Process.Start ("../../../ghostscript/gswin64c.exe", "tmp_file.ps") |> ignore
