namespace PostScriptGen

open TreeDesign
open System
open System.IO
open System.Diagnostics

module PostScriptGen =

    let rec findLargestX (Node((_, x), subtree)) : float =
        match subtree with
        | [] -> x
        | _ -> List.max (List.map findLargestX subtree)

    let toIntString (x : float) =
        string(int(x))

    let rec generateImpl (Node((label, _), subtree)) (x : float) (y : float) (maxX : float) : string =
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
                (generateImpl (Node((label2, dx), subtree2)) (x + dx / maxX) (y - 50.0) maxX)
            ]) subtree))
        ]

    let generate (designResult : Tree<string*float>) : string =
        String.concat "" [
            "%!\n<</PageSize[1000 1400]/ImagingBBox null>> setpagedevice\n/Times-Roman findfont 10 scalefont setfont\n1 1 scale\n";
            (generateImpl designResult 500.0 1340.0 ((findLargestX designResult + 20.0) / 1000.0));
            "\nshowpage"
        ]

    let psToPdfFile (psContents : string) =
        File.WriteAllText ("tmp_file.ps", psContents) |> ignore
        Process.Start ("../../../ghostscript/gswin64c.exe", "tmp_file.ps") |> ignore
