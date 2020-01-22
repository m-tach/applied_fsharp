namespace Client

open System
open System.Diagnostics
open System.Net
open System.Threading

open SharedTypes.NetworkStuff
open SharedTypes.SharedTypes 

module ClientStuff =

    type Client() =
        let newGameServerFound = new Event<GameServer>()
        let newGameState = new Event<GameState>()

        [<CLIEvent>]
        member public this.NewGameServerFoundEvent = newGameServerFound.Publish
        [<CLIEvent>]
        member public this.NewGameStateEvent = newGameState.Publish


        member public this.JoinGame(server: GameServer) : bool =
            true

        member public this.KeyPressed(key: char) =
            "something" |> ignore                  



/// Automaton for the Client, used to connect to a ping-pong game
module StateMachine = 
    let procStartInfo = 
        ProcessStartInfo(
            UseShellExecute = true,
            CreateNoWindow = false,
            FileName = "Server.exe"
        )
    let p = new Process(StartInfo = procStartInfo)

    //TODO: for now, event queue stores strings
    let ev = AsyncEventQueue<String>()
    
    // start in a lobby
    let rec start() = 
        async {
            printfn "state: start"; 

            //broadcast request available servers for lobby 
            do! Broadcast(
                    System.Text.Encoding.UTF8.GetBytes("Give me servers"), 
                    9001)

            //TODO: events received from UI        
            let! msg = ev.Receive();
            match msg with
             | "Host game"  -> return! startServerProcess()
             | "Join game"  -> return! joinGame()
             | _         -> failwith("waitingForPlayers: unexpected message")
            }

    /// start a server process to host a game
    and startServerProcess() = 
        async {
            printfn "state: startServerProcess"; 
            // start process and check that it exists
            p.Start();            
            let processes = Process.GetProcessesByName("Server");
            printfn "Started process %A " processes
            let! msg = ev.Receive();
            match msg with
             | "Two players have now joined"  -> return! joinGame()
             | _         -> failwith("startServerProcess: unexpected message")
            }

    /// shows game window; waits for two players to join
    and joinGame() = 
        async {
            //printfn "state: joinGame"; 
            let! msg = ev.Receive();
            match msg with
             | "Two players have now joined"  -> return! receiveGameState()
             | _         -> failwith("joinGame: unexpected message")
            }

    /// receives a game state and updates UI
    and receiveGameState() = 
        async {
            //printfn "state: joinGame"; 
            let! msg = ev.Receive();
            match msg with
             | "GameState"  ->
                            //TODO: send message to update UI
                            return! sendInput() //TODO: should be periodic
             | _         -> failwith("receiveGameState: unexpected message")
            }

    /// receives a key stroke and updates server
    and sendInput() = 
        async {
            //printfn "state: joinGame"; 
            let! msg = ev.Receive();
            match msg with
             | "Up"  ->
                            //TODO: send input to Server
                            return! receiveGameState()
             | _         -> failwith("sendInput: unexpected message")
            }        

