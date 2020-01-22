module App  
  
open System  
open UserInterface
  
[<EntryPoint;STAThread>]  
let main argv =
    UserInterface.run
    0