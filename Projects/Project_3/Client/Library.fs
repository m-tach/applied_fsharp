﻿namespace Client

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
        let ev = AsyncEventQueue<Message>()
        let cl = Client(ev);
        let sender = NetworkSender(9001) 
        ///nwRec listens for incoming traffic on 9001 and adds it to queue0
        let nwRec = NetworkReceiver(9001)
        do
            nwRec.StartListening();
            nwRec.ReceiveMessageEvent.Add(fun x -> ev.Post(x))
      
        member public this.InternalClient = cl

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
            p.Start() |> ignore 
            let processes = Process.GetProcessesByName("Server");
            printfn "Started process %A " processes

        member private this.StartLobby() = 
            async {
                printfn "state: start"; 
                //broadcast request available servers for lobby 
                do! Broadcast(RequestServers(getOwnIpAddress), 9001);
                let! msg = ev.Receive();
                match msg with
                 | HostGame  serverName -> this.StartServerProcess(serverName);
                                           do! sender.Send(JoinGame(getOwnIpAddress), IPAddress.Loopback) ;
                                           return! this.StartLobby()

                 | JoinGame ipAddr-> do! sender.Send(JoinGame(getOwnIpAddress), ipAddr) ;
                                     return! this.StartLobby();

                 | YouJoinedTheGame(playerId, serverAddress) -> return! this.WaitForStartGame(playerId, serverAddress);   

                 | Server gameServer -> cl.NewGameServerTrigger.Trigger(gameServer); 
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
                | _ -> return! this.WaitForStartGame(playerId, serverAddress);
            }

        /// receives a game state and updates UI
        member private this.ReceiveGameState(playerId : int, serverAddress: IPAddress) = 
            async {
                //printfn "state: receiveGameState"; 
                let! msg = ev.Receive();
                match msg with
                 | GameStateUpdate gState  ->
                                cl.NewGameStateEventTrigger.Trigger(gState);
                                do! Async.Sleep(50) 
                                return! this.SendInput(playerId, serverAddress)
                 | GameDone -> return! this.StartLobby();                    
                 | _         -> return! this.ReceiveGameState(playerId, serverAddress)
                }

        /// receives a key stroke and updates server
        /// TODO: fix/clear cl.KeyInput
        member private this.SendInput(playerId : int, serverAddress: IPAddress) = 
            async {
                do! sender.Send(PlayerInput(playerId, cl.KeyInput), serverAddress)
                return! this.ReceiveGameState(playerId, serverAddress)
            }        

