namespace Client

open System
open System.Diagnostics
open System.Net
open System.Threading

open SharedTypes.NetworkStuff
open SharedTypes.SharedTypes 

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

        member public this.HostGame(serverName: string) =
            stateMachineQueue.Post(HostGame(serverName))
            
        member public this.KeyPressed(input: Input) =
            keyInput <- input                              



    /// Automaton for the Client, used to connect to a ping-pong game
    type ClientStateMachine() = 
        ///ev is a queue, which stores messages in order they have been received
        let ev = AsyncEventQueue<SharedTypes.SharedTypes.Message>()
        let cl = Client(ev);
        let mutable nwSender = NetworkSender(9001, IPAddress.Loopback) 
        ///nwRec listens for incoming traffic on 9001 and adds it to queue0
        let nwRec = NetworkReceiver(9001)
        do
            nwRec.StartListening();
            nwRec.ReceiveMessageEvent.Add(fun x -> ev.Post(x))
      
        member public this.InternalClient = cl
        
        /// start a server process to host a game
        member private this.StartServerProcess(serverName: string) = 
            let procStartInfo = 
                ProcessStartInfo(
                    UseShellExecute = true,
                    CreateNoWindow = false,
                    FileName = "Server.exe",
                    Arguments = serverName
                )
            let p = new Process(StartInfo = procStartInfo)
            printfn "state: StartServerProcess"; 
            p.Start() ;            
            let processes = Process.GetProcessesByName("Server");
            printfn "Started process %A " processes





        /// Start in a lobby
        /// Flow:
        /// 1. Broadcast request available servers for lobby
        /// 2. Process incoming messages from queue
        member private this.startLobby() = 
            async {
                printfn "state: start"; 
                //broadcast request available servers for lobby 
                do! Broadcast(RequestServers(getOwnIpAddress), 9001);
                let! msg = ev.Receive();
                match msg with
                 | HostGame  serverName -> this.StartServerProcess(serverName);
                                           nwSender <- NetworkSender(9001, IPAddress.Loopback);
                                           do! nwSender.Send(JoinGame(getOwnIpAddress)) ;
                                           return! this.startLobby()

                 | JoinGame ipAddr-> nwSender <- NetworkSender(9001, ipAddr);
                                     do! nwSender.Send(JoinGame(getOwnIpAddress)) ;
                                     return! this.startLobby();

                 | YouJoinedTheGame playerId-> return! this.waitForStartGame(playerId);   

                 | Server gameServer -> cl.NewGameServerTrigger.Trigger(gameServer); 
                                        return! this.startLobby();
                                        
                 | _         -> printfn "start: unexpected message %A" msg; 
                                return! this.startLobby();
                }
        
        /// screen shows player names + "Waiting for start game"
        member private this.waitForStartGame(playerId : int) = 
            async {
                printfn "state: waitForStartGame for player id %d" playerId; 
                cl.WaitForStartGameTrigger.Trigger(playerId);    
                let! msg = ev.Receive();
                match msg with 
                | StartGame  -> cl.LaunchGameTrigger.Trigger(); return! this.sendInput(playerId);
                | _ -> return! this.waitForStartGame(playerId);
            }

        /// receives a game state and updates UI
        member private this.receiveGameState(playerId: int) = 
            async {
                //printfn "state: receiveGameState"; 
                let! msg = ev.Receive();
                match msg with
                 | GameStateUpdate gState  ->
                                cl.NewGameStateEventTrigger.Trigger(gState);
                                do! Async.Sleep(50) 
                                return! this.sendInput(playerId)
                 | GameDone -> return! this.startLobby();                    
                 | _         -> failwith("receiveGameState: unexpected message")
                }

        /// receives a key stroke and updates server
        /// TODO: fix/clear cl.KeyInput
        member private this.sendInput(playerId: int) = 
            async {
                do! nwSender.Send(PlayerInput(playerId, cl.KeyInput))
                return! this.receiveGameState(playerId)
            }        

