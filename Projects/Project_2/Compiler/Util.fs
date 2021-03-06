﻿// Michael R. Hansen 05-01-2016, 04-01-2018

namespace GuardedCommands.Util

open System
open System.IO
open System.Text
open Microsoft.FSharp.Text.Lexing

open GuardedCommands.Frontend.AST
open Parser
open Lexer
 
open Machine
open VirtualMachine

//open GuardedCommands
//open GuardedCommands
//open GuardedCommands.Backend
//open Backend.CodeGeneration
//open Backend.CodeGenerationOpt
//open Backend.CodeGeneration
//open GuardedCommands.Frontend
//open GuardedCommands.Frontend.TypeCheck




module ParserUtil = 

   let parseString (text:string) =
      let lexbuf = LexBuffer<_>.FromBytes(Encoding.UTF8.GetBytes(text))
      try
           Main Lexer.tokenize lexbuf
      with e ->
           let pos = lexbuf.EndPos
           printfn "Error near line %d, character %d\n" pos.Line pos.Column
           failwith "parser termination"


// Parse a file. (A statement is parsed) 
   let parseFromFile filename =
      if File.Exists(filename)    
      then parseString(File.ReadAllText(filename))
      else invalidArg "ParserUtil" "File not found"

open ParserUtil

open GuardedCommands.Backend
open GuardedCommands.Frontend.TypeCheck

module CompilerUtil =

/// goOpt p compiles (using the optimized version) and runs an abstract syntax for a program  
   let goOpt p = run(code2ints(CodeGenerationOpt.CP p))

/// go p compiles and runs an abstract syntax for a program  
   let go p = run(code2ints(CodeGeneration.CP p))

/// goOpt p compile and runs an abstract syntax for a program showing a program trace  
   let goTrace p = runTrace(code2ints(CodeGeneration.CP p))

/// goBenchmark p compile and runs an abstract syntax for a program returning amount of executed instructions and execution time 
   let goBenchmark p = runBenchmark(code2ints(CodeGeneration.CP p))

/// exec filename parses, type checks, compiles and runs a program in a file
   let exec filename =  printfn "\nParse, typecheck, compilation and execution of %s:" filename 
                        let prog = parseFromFile filename
                        tcP prog
                        go prog

/// execOpt filename parses, type checks, compiles and runs a program in a file
   let execOpt filename =  printfn "\nParse, typecheck, optimized compilation and execution of %s:" filename 
                           let prog = parseFromFile filename
                           tcP prog
                           goOpt prog

/// execTrace filename parses, type checks, compiles and runs a program in a file showing a program trace
   let execTrace filename =  printfn "\nParse, typecheck, compilation and execution of %s:" filename 
                             let prog = parseFromFile filename
                             tcP prog
                             goTrace prog

   let instrsToString instrs = 
       (String.concat "\n" (List.map (fun x ->match x with
                                              | CSTI i -> String.Format("CSTI {0}", i)
                                              | ADD -> "ADD"
                                              | SUB -> "SUB"
                                              | MUL -> "MUL"
                                              | DIV -> "DIV"
                                              | MOD -> "MOD"
                                              | EQ -> "EQ"
                                              | LT -> "LT"
                                              | NOT -> "NOT"
                                              | DUP -> "DUP"
                                              | SWAP -> "SWAP"
                                              | LDI -> "LDI"
                                              | STI -> "STI"
                                              | GETBP -> "GETBP"
                                              | GETSP -> "GETSP"
                                              | INCSP m -> String.Format("INCSP {0}", m)
                                              | GOTO a -> String.Format("GOTO {0}", a)
                                              | IFZERO a -> String.Format("IFZERO {0}", a)
                                              | IFNZRO a -> String.Format("IFNZRO {0}", a)
                                              | CALL(m, a) -> String.Format("CALL {0} {1}", m, a)
                                              | TCALL(m, n, a) -> String.Format("TCALL {0} {1} {2}", m, n, a)
                                              | RET m -> String.Format("RET {0}", m)
                                              | PRINTI -> "PRINTI"
                                              | PRINTC -> "PRINTC"
                                              | LDARGS -> "LDARGS"
                                              | STOP -> "STOP"
                                              | Label a -> String.Format("Label {0}", a)) instrs))   

/// execBenchmark filename parses, type checks, compiles and runs a program, returning the amount of executed instructions and the runtime in ms
   let execBenchmark filename = printfn "\nParse, typecheck, compilation and execution of %s:" filename 
                                let prog = parseFromFile filename
                                tcP prog
                                goBenchmark prog
(*
   let exec str  = let prog = parseString str
                   Frontend.TypeCheck.tcP prog;
                   go prog *)
(*
// Parse a string. (A declaration list is parsed)  
   let parseDecList (text:string) =
      let lexbuf = LexBuffer<_>.FromBytes(Encoding.UTF8.GetBytes(text))
      try
          Parser.DecList Lexer.tokenize lexbuf
      with e ->
           let pos = lexbuf.EndPos
           printfn "Error near line %d, character %d\n" pos.Line pos.Column
           failwith "parser termination"

// Parse a file. (A declaration list is parsed) 
   let parseDecListFromFile filename =
     if File.Exists(filename)    
      then parseDecList(File.ReadAllText(filename))
      else invalidArg "ParserUtil" "File not found"


   let parseExp (text:string) =
      let lexbuf = LexBuffer<_>.FromBytes(Encoding.UTF8.GetBytes(text))
      try
          Parser.Exp Lexer.tokenize lexbuf
      with e ->
           let pos = lexbuf.EndPos
           printfn "Error near line %d, character %d\n" pos.Line pos.Column
           failwith "parser termination"


   let parseDec (text:string) =
      let lexbuf = LexBuffer<_>.FromBytes(Encoding.UTF8.GetBytes(text))
      try
          Parser.Dec Lexer.tokenize lexbuf
      with e ->
           let pos = lexbuf.EndPos
           printfn "Error near line %d, character %d\n" pos.Line pos.Column
           failwith "parser termination"

   let parseStm (text:string) =
      let lexbuf = LexBuffer<_>.FromBytes(Encoding.UTF8.GetBytes(text))
      try
          Parser.Stm Lexer.tokenize lexbuf
      with e ->
           let pos = lexbuf.EndPos
           printfn "Error near line %d, character %d\n" pos.Line pos.Column
           failwith "parser termination"

   let parseStmList (text:string) =
      let lexbuf = LexBuffer<_>.FromBytes(Encoding.UTF8.GetBytes(text))
      try
          Parser.StmList Lexer.tokenize lexbuf
      with e ->
           let pos = lexbuf.EndPos
           printfn "Error near line %d, character %d\n" pos.Line pos.Column
           failwith "parser termination"

   let parseProgram (text:string) =
      let lexbuf = LexBuffer<_>.FromBytes(Encoding.UTF8.GetBytes(text))
      try
          Parser.Prog Lexer.tokenize lexbuf
      with e ->
           let pos = lexbuf.EndPos
           printfn "Error near line %d, character %d\n" pos.Line pos.Column
           failwith "parser termination"

*)


(*

// Compile, type check, generate code and run a program from a file
   let goFile file = go (parseFromFile file)

   let intsFromMain(args: string[]) = 
         let file = args.[0]
         let ps = Array.map int (args.[1..]) 
         let p = parseFromFile file
         (code2ints(CP p), ps)

   let goArgsTrace (args: string[]) = 
         let (ints,ps) = intsFromMain args
         VirtualMachine.runArgsTrace ints ps 
         
   let goArgs (args: string[]) = 
         let (ints,ps) = intsFromMain args
         VirtualMachine.runArgs ints ps
  *)