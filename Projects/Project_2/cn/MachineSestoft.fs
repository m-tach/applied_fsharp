module MachineSestoft

let CODECSTI   = 0 
let CODEADD    = 1 
let CODESUB    = 2 
let CODEMUL    = 3 
let CODEDIV    = 4 
let CODEMOD    = 5 
let CODEEQ     = 6 
let CODELT     = 7 
let CODENOT    = 8 
let CODEDUP    = 9 
let CODESWAP   = 10 
let CODELDI    = 11 
let CODESTI    = 12 
let CODEGETBP  = 13 
let CODEGETSP  = 14 
let CODEINCSP  = 15 
let CODEGOTO   = 16
let CODEIFZERO = 17
let CODEIFNZRO = 18 
let CODECALL   = 19
let CODETCALL  = 20
let CODERET    = 21
let CODEPRINTI = 22 
let CODEPRINTC = 23
let CODELDARGS = 24
let CODESTOP   = 25


let STACKSIZE = 1000
  

let f x = x+1
  // The machine: execute the code starting at p[pc] 

let execcode(p: int[], s: int[], iargs: int[], trace: bool) = 
  let mutable bp = -999 
  let mutable sp = -1
  let mutable pc = 0 
  let execStep i = 
    match i with
    | i when i=CODECSTI -> s.[sp+1] <- p.[pc+1]; sp <- sp+1; pc <- pc+2
(*    | i when i=CODEADD  -> s.[sp-1] <- s.[sp-1] + s.[sp]; sp <- sp-1; pc <- pc+1
    | i when i=CODESUB  -> s.[sp-1] <- s.[sp-1] - s.[sp]; sp <- sp-1; pc <- pc+1
    | i when i=CODEMUL  -> s.[sp-1] <- s.[sp-1] * s.[sp]; sp <- sp-1; pc <- pc+1
    | i when i=CODEDIV  -> s.[sp-1] <- s.[sp-1] / s.[sp]; sp <- sp-1; pc <- pc+1
    | i when i=CODEMOD  -> s.[sp-1] <- s.[sp-1] % s.[sp]; sp <- sp-1; pc <- pc+1
    | i when i=CODEEQ   -> s.[sp-1] <- (if s.[sp-1] = s.[sp] then 1 else 0); sp <- sp-1; pc <- pc+1
    | i when i=CODELT   -> s.[sp-1] <- (if s.[sp-1] < s.[sp] then 1 else 0); sp <- sp-1; pc <- pc+1
    | i when i=CODENOT  -> s.[sp] <- if s.[sp] = 0 then 1 else 0
    | i when i=CODEDUP  -> s.[sp+1] <- s.[sp]; sp <- sp+1; pc <- pc+1
    | i when i=CODESWAP -> let tmp = s.[sp]  
                           s.[sp] <- s.[sp-1];  s.[sp-1] <- tmp 
    | i when i=CODELDI  -> s.[sp] <- s.[s.[sp]]; pc <- pc+1                                  // load indirect
        
    | i when i=CODESTI  -> s.[s.[sp-1]] <- s.[sp]; s.[sp-1] <- s.[sp]; sp <- sp-1; pc <- pc+1 // store indirect, keep value on top
    | i when i=CODEINCSP -> sp <- sp+p.[pc+1]; pc <- pc+2
    | i when i=CODEGOTO  -> pc <- p.[pc+1]
    | i when i=CODEIFZERO -> (pc <- if s.[sp] = 0 then p.[pc+1] else pc+2); sp <- sp-1
    | i when i=CODEIFNZRO -> (pc <- if s.[sp] <> 0 then p.[pc+1] else pc+2); sp <- sp-1        
    | i when i=CODEPRINTI -> System.Console.WriteLine(string s.[sp] + " ") ; pc <- pc+1  
    | i when i=CODEPRINTC -> System.Console.WriteLine((char)(s.[sp])); pc <- pc+1 *)
    | _                   -> ()  



  let rec exec() = let i = p.[pc]
                   if i = CODESTOP then sp
                   else ((execStep i) ; exec())
  exec()  
 
 
  // Print the stack machine instruction at p[pc]

let insname(p: int[], pc: int)  =
    match p.[pc] with
    | i when i=CODECSTI -> "CSTI " + (string p.[pc+1]) 
    | i when i=CODEADD  -> "ADD"
    | i when i=CODESUB  -> "SUB"
    | i when i=CODEMUL  -> "MUL"
    | i when i=CODEDIV  -> "DIV"
    | i when i=CODEMOD  -> "MOD"
    | i when i=CODEEQ   -> "EQ"
    | i when i=CODELT   -> "LT"
    | i when i=CODENOT  -> "NOT"
    | i when i=CODEDUP  -> "DUP"
    | i when i=CODESWAP -> "SWAP"
    | i when i=CODELDI  -> "LDI"
    | i when i=CODESTI  -> "STI"
    | i when i=CODEGETBP ->"GETBP"
    | i when i=CODEGETSP -> "GETSP"
    | i when i=CODEINCSP -> "INCSP " + (string p.[pc+1])
    | i when i=CODEGOTO  -> "GOTO " + (string p.[pc+1])
    | i when i=CODEIFZERO -> "IFZERO " + (string p.[pc+1])
    | i when i=CODEIFNZRO -> "IFNZRO " + (string p.[pc+1])
    | i when i=CODECALL   -> "CALL " + (string p.[pc+1]) + " " + (string p.[pc+2])
    | i when i=CODETCALL  -> "TCALL " + (string p.[pc+1]) + " " + (string p.[pc+2]) + " " + (string p.[pc+3])
    | i when i=CODERET    -> "RET " + (string p.[pc+1])
    | i when i=CODEPRINTI -> "PRINTI"
    | i when i=CODEPRINTC -> "PRINTC"
    | i when i=CODELDARGS -> "LDARGS"
    | i when i=CODESTOP   -> "STOP"
    | _      -> "<unknown>";;


  // Print current stack and current instruction

let printsppc(s: int[], bp: int, sp: int, p: int[], pc: int) =
    let mutable i = 0
    System.Console.Write("[ ")
    while i<=sp do
      System.Console.Write((string(s.[i])) + " ")
    System.Console.Write("]")
    System.Console.WriteLine("{" + string pc + ": " + insname(p, pc) + "}")
 
