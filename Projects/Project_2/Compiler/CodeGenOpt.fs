namespace GuardedCommands.Backend
// Michael R. Hansen 05-01-2016, 04-01-2018

open System
open Machine

open GuardedCommands.Frontend.AST
open GuardedCommands.Backend.Optimizer
open GuardedCommands.Backend.CodeGeneration

module CodeGenerationOpt =

(* Compile a complete micro-C program: globals, call to main, functions *)

   let CP p = Optimizer.optimize (CodeGeneration.CP p)



