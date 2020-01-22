namespace Client

open System
open System.Diagnostics
open System.Net
open System.Threading

open SharedTypes.NetworkStuff
open SharedTypes.SharedTypes 

module StartServerHelper =  
    let procStartInfo = 
        ProcessStartInfo(
            UseShellExecute = true,
            CreateNoWindow = false,
            FileName = "Server.exe"
        )
    let p = new Process(StartInfo = procStartInfo)

module ClientStuff =

    type Client(stateMachineQueue:  AsyncEventQueue<Message>) =
        let newGameServerFound = new Event<GameServer>()
        let newGameState = new Event<GameState>()
        let waitForStartGame = new Event<int>()
        let launchGame = new Event<_>()
        let mutable keyInput = Up

        member public this.KeyInput = keyInput
        
        
        [<CLIEvent>]
        member public this.NewGameServerFoundEvent = newGameServerFound.Publish
        member public this.NewGameServerTrigger = newGameServerFound

        [<CLIEvent>]
        member public this.NewGameStateEvent = newGameState.Publish
        member public this.NewGameStateEventTrigger = newGameState

        [<CLIEvent>]
        member public this.WaitForStartGameEvent = waitForStartGame.Publish
        member public this.WaitForStartGameTrigger = waitForStartGame
        
        [<CLIEvent>]
        member public this.LaunchGameEvent = launchGame.Publish
        member public this.LaunchGameTrigger = launchGame


        member public this.JoinGame(server: GameServer) =
            stateMachineQueue.Post(JoinGame(server.Address))
            
        member public this.KeyPressed(input: Input) =
            keyInput <- input                              



/// Automaton for the Client, used to connect to a ping-pong game
module StateMachine = 
    /// start a server process to host a game
    let startServerProcess() = 
        printfn "state: startServerProcess"; 
        StartServerHelper.p.Start();            
        let processes = Process.GetProcessesByName("Server");
        printfn "Started process %A " processes


    ///ev is a queue, which stores messages in order they have been received
    let ev = AsyncEventQueue<SharedTypes.SharedTypes.Message>()
    ///nwRec listens for incoming traffic on 9001 and adds it to queue
    let nwRec = NetworkReceiver(9001)
    nwRec.StartListening();
    nwRec.ReceiveMessageEvent.Add(fun x -> ev.Post(x))
    let cl = ClientStuff.Client(ev);
    let mutable nwSender = NetworkSender(9001, IPAddress.Loopback) 

    /// Start in a lobby
    /// Flow:
    /// 1. Broadcast request available servers for lobby
    /// 2. Process incoming messages from queue
    let rec startLobby() = 
        async {
            printfn "state: start"; 
            //broadcast request available servers for lobby 
            do! Broadcast(RequestServers, 9001);
            let! msg = ev.Receive();
            match msg with
             | HostGame  -> startServerProcess();
                            nwSender <- NetworkSender(9001, IPAddress.Loopback);
                            do! nwSender.Send(JoinGame(getOwnIpAddress)) ;
                            return! startLobby()

             | JoinGame ipAddr-> nwSender <- NetworkSender(9001, ipAddr);
                                 do! nwSender.Send(JoinGame(getOwnIpAddress)) ;
                                 return! startLobby();

             | YouJoinedTheGame playerId-> return! waitForStartGame(playerId);   

             | Server gameServer -> cl.NewGameServerTrigger.Trigger(gameServer); 
                                    return! startLobby();
                                    
             | _         -> printfn "start: unexpected message %A" msg; 
                            return! startLobby();
            }
    
    /// screen shows player names + "Waiting for start game"
    and waitForStartGame(playerId : int) = 
        async {
            printfn "state: waitForStartGame for player id %d" playerId; 
            cl.WaitForStartGameTrigger.Trigger(playerId);    
            let! msg = ev.Receive();
            match msg with 
            | StartGame  -> cl.LaunchGameTrigger.Trigger(); return! sendInput(playerId);
            | _ -> return! waitForStartGame(playerId);
        }

    /// receives a game state and updates UI
    and receiveGameState(playerId: int) = 
        async {
            //printfn "state: receiveGameState"; 
            let! msg = ev.Receive();
            match msg with
             | GameStateUpdate gState  ->
                            cl.NewGameStateEventTrigger.Trigger(gState);
                            do! Async.Sleep(50) 
                            return! sendInput(playerId)
             | GameDone -> return! startLobby();                    
             | _         -> failwith("receiveGameState: unexpected message")
            }

    /// receives a key stroke and updates server
    /// TODO: fix/clear cl.KeyInput
    and sendInput(playerId: int) = 
        async {
            do! nwSender.Send(PlayerInput(playerId, cl.KeyInput))
            return! receiveGameState(playerId)
        }        

