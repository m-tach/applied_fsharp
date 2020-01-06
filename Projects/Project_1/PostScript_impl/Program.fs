type Tree<'x> = Node of 'x * Tree<'x> list;;

type PageSetup = (float * float);;

let rec findLargestX (Node((_, x), subtree)) : float =
    match subtree with
    | [] -> x
    | _ -> List.max (List.map findLargestX subtree)

let rec generateImpl (Node((label, x), subtree)) (y : float) (maxX : float) : string =
    String.concat "" [
        string(int(x / maxX * 800.0));
        " ";
        string(int(y));
        " moveto\n(";
        label;
        ") dup stringwidth pop 2 div neg 0 rmoveto show";
        (String.concat "\n" (List.map (fun (Node((label2, x2), subtree2)) -> String.concat "" [
            "\nnewpath\n";
            string(int(x / maxX * 800.0));
            " ";
            string(int(y - 16.0));
            " moveto\n";
            string(int(x / maxX * 800.0));
            " ";
            string(int(y - 25.0));
            " lineto\n";
            string(int(x2 / maxX * 800.0));
            " ";
            string(int(y - 25.0));
            " lineto\n";
            string(int(x2 / maxX * 800.0));
            " ";
            string(int(y - 40.0));
            " lineto\nstroke\n";
            (generateImpl (Node((label2, x2), subtree2)) (y - 50.0) maxX)
        ]) subtree))
    ]

let generate (designResult : Tree<string*float>) : string =
    String.concat "" [
        "%!\n<</PageSize[1000 1400]/ImagingBBox null>> setpagedevice\n/Times-Roman findfont 10 scalefont setfont\n1 1 scale\n";
        (generateImpl designResult (842.0 - 30.0) (findLargestX designResult));
        "\nshowpage"
    ]

[<EntryPoint>]
let main argv =
    let tree = Node(("hello", 10.0), [
        Node(("world", 5.0), [
            Node(("abc", 2.0), []);
            Node(("def", 8.0), [])]
        );
        Node(("f#", 15.0), [])
    ])
    let result = generate tree
    printfn "%s" result
    0 // return an integer exit code