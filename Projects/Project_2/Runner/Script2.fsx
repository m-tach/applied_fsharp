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


let ex0Tree = parseFromFile "../programs/VarDefinedTwiceFail.gc";;

let _ = tcP ex0Tree
go ex0Tree
printf "done."