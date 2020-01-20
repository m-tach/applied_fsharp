// Michael R. Hansen 05-01-2016; Revised 04-01-2018
#I @"..\Compiler"
#r @"bin\Debug\net48\Machine.dll"
#r @"bin\Debug\net48\Compiler.dll"
#r @"bin\Debug\net48\VirtualMachine.dll"

// You must revise the following 3 pathes 
open GuardedCommands.Util
open GuardedCommands.Frontend.TypeCheck
open GuardedCommands.Frontend.AST
open GuardedCommands.Backend.CodeGeneration

open ParserUtil
open CompilerUtil

open Machine
open VirtualMachine

// The Ex0.gc example:


let ex0Tree = parseFromFile "../programs/Ex0.gc";;

let _ = tcP ex0Tree;;

let ex0Code = CP ex0Tree;; 

let _ = go ex0Tree;;

let _ = goTrace ex0Tree;;


// Parsing of Ex1.gc

let ex1Tree = parseFromFile "../programs/Ex1.gc";; 

// -- is typechecked as follows:

let _ = tcP ex1Tree;;

// obtain symbolic code:
let ex1Code = CP ex1Tree;; 

// -- is executed with trace as follows:
let stack = goTrace ex1Tree;;

// -- is executed as follows (no trace):
let sameStack = go ex1Tree;;

// "All in one" parse from file, type check, compile and run 

let _ = exec "../programs/Ex1.gc";;

let _ = exec "../programs/Ex2.gc";;

// Test of programs covered by the fifth task using optimized compilation (Section 8.2):
List.iter execOpt ["../programs/Ex1.gc"; "../programs/Ex2.gc"];;

// All programs relating to the basic version can be parsed:
let pts = List.map parseFromFile [
    "../programs/Ex1.gc";
    "../programs/Ex2.gc";
    "../programs/Ex3.gc";
    "../programs/Ex4.gc";
    "../programs/Ex5.gc";
    "../programs/Ex6.gc";
    "../programs/Skip.gc"
];;

// The parse tree for Ex3.gc
List.item 2 pts ;;

// Test of programs covered by the first task (Section 3.7):
List.iter exec ["../programs/Ex1.gc"; "../programs/Ex2.gc";"../programs/Ex3.gc"; "../programs/Ex4.gc"; "../programs/Ex5.gc"; "../programs/Ex6.gc"; "../programs/Skip.gc"];;

// Test of programs covered by the second task (Section 4.3):
List.iter exec ["../programs/Ex7.gc"; "../programs/fact.gc"; "../programs/factRec.gc"; "../programs/factCBV.gc"];;

// Test of programs covered by the fourth task (Section 5.4):
List.iter exec ["../programs/A0.gc"; "../programs/A1.gc"; "../programs/A2.gc"; "../programs/A3.gc"];;

// Test of programs covered by the fifth task (Section 6.1):
List.iter exec ["../programs/A4.gc"; "../programs/Swap.gc"; "../programs/QuickSortV1.gc"];;

// Test of programs covered by the fifth task (Section 7.4):
List.iter exec ["../programs/par1.gc"; "../programs/factImpPTyp.gc"; "../programs/QuickSortV2.gc"; "../programs/par2.gc"];;

// Test of programs covered by the fifth task using optimized compilation (Section 8.2):
List.iter execOpt ["../programs/par1.gc"; "../programs/factImpPTyp.gc"; "../programs/QuickSortV2.gc"; "../programs/par2.gc"];;

// ^ TO-DO: Get optimizations into master branch, then update execOpt to use the two-step optimization