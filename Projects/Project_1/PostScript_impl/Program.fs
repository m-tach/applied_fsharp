namespace PostScriptGen

open TreeDesign
open System
open System.IO
open System.Diagnostics
open System.Text

module PostScriptGen =

    // Finds the largest x coordinate (width/2) of the tree and its descendant nodes.
    let rec findLargestX (Node((_, x), subtree)) : float =
        match subtree with
        | [] -> x
        | _ -> x + List.max (List.map findLargestX subtree)

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
                    yield (toIntString (y - 8.0))
                    yield " moveto\n"
                    yield (toIntString x)
                    yield " "
                    yield (toIntString (y - 25.0))
                    yield " lineto\n"
                    yield (toIntString (x + dx / maxX))
                    yield " "
                    yield (toIntString (y - 25.0))
                    yield " lineto\n"
                    yield (toIntString (x + dx / maxX))
                    yield " "
                    yield (toIntString (y - 40.0))
                    yield " lineto\nstroke\n"
                    yield! generateImpl (Node((label2, dx), subtree2)) (x + dx / maxX) (y - 50.0) maxX
                }
            ) subtree)
        }

    // The main entry for converting a design-parsed tree into PostScript commands.
    let generate (designResult : Tree<string*float>) : string =
        String.concat "" (Seq.toList (seq {
            yield "%!\n<</PageSize[1000 1400]/ImagingBBox null>> setpagedevice\n/Times-Roman findfont 10 scalefont setfont\n1 1 scale\n"
            yield! (generateImpl designResult 500.0 1340.0 ((findLargestX designResult + 20.0) / 1000.0))
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
        let s = StringBuilder()
        s.Append "%!\n<</PageSize[1000 1400]/ImagingBBox null>> setpagedevice\n/Times-Roman findfont 10 scalefont setfont\n1 1 scale\n" |> ignore
        generateImplBuilder s designResult 500.0 1340.0 ((findLargestX designResult + 20.0) / 1000.0)
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
        String.concat "" [
            "%!\n<</PageSize[1000 1400]/ImagingBBox null>> setpagedevice\n/Times-Roman findfont 10 scalefont setfont\n1 1 scale\n";
            (generateImplConcat designResult 500.0 1340.0 ((findLargestX designResult + 20.0) / 1000.0));
            "\nshowpage"
        ]

    // Additional function to save the PostScript to a file and open external library GhostScript to view the visual tree.
    // TYPE 'quit' TWICE TO EXIT!
    let psToPdfFile (psContents : string) =
        File.WriteAllText ("tmp_file.ps", psContents) |> ignore
        Process.Start ("../../../ghostscript/gswin64c.exe", "tmp_file.ps") |> ignore
