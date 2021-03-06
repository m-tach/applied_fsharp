﻿namespace Client

open System
open System.Diagnostics
open System.Net
open System.Threading

open SharedTypes.NetworkStuff
open SharedTypes.SharedTypes 
open SharedTypes.Constants

module ClientStuff =

 ///Type Client is an intermediary between UI and State machine 
    type Client() as this =
        let stateMachineQueue = AsyncEventQueue<Message>()
        let newGameServerFound = new Event<GameServer>()
        let newGameState = new Event<GameState>()
        let waitForStartGame = new Event<int>()
        let launchGame = new Event<_>()
        let goToLobby = new Event<_>()

        let mutable keyInput = Up
        do 
            let csm = ClientStateMachine(this, stateMachineQueue);
            csm.StartStateMachine();

        member public this.KeyInput = keyInput
        
        //Events for communication with WPF
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

        [<CLIEvent>]
        member public this.GoToLobbyEvent = goToLobby.Publish
        member public this.GoToLobbyTrigger = goToLobby


        member public this.JoinGame(server: GameServer) =
            stateMachineQueue.Post(JoinGame(server.Address))

        member public this.HostGame(serverName: string) =
            stateMachineQueue.Post(HostGame(serverName, false))

        member public this.HostGameComputer() =
            stateMachineQueue.Post(HostGame("-", true))

        member public this.ExitGame() =
            stateMachineQueue.Post(GameDone)

        member public this.BroadcastRequestServers() =
            stateMachineQueue.Post(BroadcastRequestServers)
            
        member public this.KeyPressed(input: Input) =
            keyInput <- input                              

    /// Automaton for the Client, used to connect to a ping-pong game
    /// ev is a queue, which stores messages in order they have been received
    and ClientStateMachine(cl: Client, ev: AsyncEventQueue<Message>) = 
        let sender = NetworkSender(SERVER_PORT) 
        ///nwRec listens for incoming traffic on CLIENT_PORT and adds it to queue0
        let nwRec = NetworkReceiver(CLIENT_PORT)
        do
            nwRec.StartListening();
            nwRec.ReceiveMessageEvent.Add(fun x -> ev.Post(x))

        (*
            #############################
            ####### State machine #######            
            #############################

               /-----------------------\
               |                       |
               |   /------\ Server     |
               V   V      | JoinGame   |
            StartLobby ---/ HostGame   |
              |                        |
              | YouJoinedTheGame       |
              V                        |
            WaitForStartGame           |
              |                        | 
              | StartGame              |
              V                        |
            SendInput                  |
              |   ^                    |
              |   | GameStateUpdate    |
              V   |                    | GameDone
            ReceiveGameState ----------/ 
        *)            


        
        /// start a server process to host a game
        member private this.StartServerProcess(serverName: string, againstComputer: bool) = 
            let procStartInfo = 
                ProcessStartInfo(
                    UseShellExecute = false,
                    CreateNoWindow = false,
                    FileName = "Server.exe",
                    Arguments = "\"" + serverName + "\"" + " " + againstComputer.ToString()
                )
            let p = new Process(StartInfo = procStartInfo)
            printfn "state: StartServerProcess"; 
            p.Start() |> ignore
            let processes = Process.GetProcessesByName("Server");
            printfn "Started process %A " processes

        member public this.StartStateMachine() =
            Async.StartImmediate (async { 
                try
                    do! this.StartLobby()
                with
                | e -> Console.Error.WriteLine (String.Format("exception: {0}\n{1}", e.Message, e.StackTrace))                                
            })
            

        member private this.StartLobby() = 
            async {
                printfn "state: start"; 
                cl.GoToLobbyTrigger.Trigger()                
                let! msg = ev.Receive();
                match msg with
                 | HostGame(serverName, againstComputer) -> this.StartServerProcess(serverName, againstComputer);
                                                            do! Async.Sleep(2000)
                                                            do! sender.Send(JoinGame(getOwnIpAddress), IPAddress.Loopback) ;
                                                            return! this.StartLobby()

                 | JoinGame ipAddr-> do! sender.Send(JoinGame(getOwnIpAddress), ipAddr) ;
                                     return! this.StartLobby();

                 | YouJoinedTheGame(playerId, serverAddress) -> return! this.WaitForStartGame(playerId, serverAddress);   

                 | Server gameServer -> cl.NewGameServerTrigger.Trigger(gameServer); 
                                        return! this.StartLobby();

                 | BroadcastRequestServers -> do! Broadcast(SERVER_PORT);
                                              return! this.StartLobby();                            
                                        
                 | _         -> printfn "start: unexpected message %A" msg; 
                                return! this.StartLobby();
                }
        
        /// screen shows player names + "Waiting for start game"
        member private this.WaitForStartGame(playerId : int, serverAddress: IPAddress) = 
            async {
                printfn "state: waitForStartGame for player id %d" playerId; 
                cl.WaitForStartGameTrigger.Trigger(playerId);    
                let! msg = ev.Receive();
                match msg with 
                | StartGame  -> cl.LaunchGameTrigger.Trigger(); return! this.SendInput(playerId, serverAddress);
                | GameDone -> return! this.StartLobby();
                | _ -> return! this.WaitForStartGame(playerId, serverAddress);
            }

        /// receives a game state and updates UI
        member private this.ReceiveGameState(playerId : int, serverAddress: IPAddress) = 
            async {
                //printfn "state: receiveGameState"; 
                use cancel = new CancellationTokenSource()
                let token = cancel.Token
                Async.Start (async {
                    do! Async.Sleep(int(TimeSpan.FromSeconds(2.0).TotalMilliseconds))
                    if not token.IsCancellationRequested
                    then ev.Post(GameDone)
                })

                let! msg = ev.Receive();
                cancel.Cancel()
                match msg with
                 | GameStateUpdate gState  ->
                                cl.NewGameStateEventTrigger.Trigger(gState); 
                                return! this.SendInput(playerId, serverAddress)
                 | GameDone -> return! this.StartLobby();                    
                 | _         -> return! this.ReceiveGameState(playerId, serverAddress)
                }

        /// receives a key stroke and updates server
        member private this.SendInput(playerId : int, serverAddress: IPAddress) = 
            async {
                do! sender.Send(PlayerInput(playerId, cl.KeyInput), serverAddress)
                return! this.ReceiveGameState(playerId, serverAddress)
            }        

   