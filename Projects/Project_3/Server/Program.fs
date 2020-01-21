// Learn more about F# at http://fsharp.org

open System

/// Automaton for the Server, hosting a ping-pong game
let rec start() = 
    async {printfn "start"; return! waitingForPlayers()}    

and waitingForPlayers() = 
    async {printfn "waitingForPlayers"; return! playGame()}    

and playGame() = 
    async {printfn "playGame"; return! sendNewState()}

and sendNewState() = 
    async {printfn "sendNewState"; return! waitForClientInput()}   

and waitForClientInput() = 
    async {printfn "waitForClientInput"; return! sendNewState()}    

/// call "Start" to launch a new Server 
[<EntryPoint>]
let main argv =
    printfn "Main"
    Async.StartImmediate (start())
    0 // return an integer exit code
