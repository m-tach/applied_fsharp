namespace Server
// Learn more about F# at http://fsharp.org

    open System
    open System.Net
    open System.Threading

    open SharedTypes.NetworkStuff
    open SharedTypes.SharedTypes 
    open GameEngine

    /// Automaton for the Server, hosting a ping-pong game
    /// TODO: make state machine a type
    module ServerStateMachine = 
        let ev = AsyncEventQueue<SharedTypes.SharedTypes.Message>()
        ///nwRec listens for incoming traffic on 9001 and adds it to queue
        let nwRec = NetworkReceiver(9001)
        nwRec.StartListening();
        nwRec.ReceiveMessageEvent.Add(fun x -> ev.Post(x))
        //nwSender for broadcasts
        let mutable nwSender = NetworkSender(9001, IPAddress.Loopback) 
        let mutable nwSenderPlayer1 = NetworkSender(9001, IPAddress.Loopback) 
        let mutable nwSenderPlayer2 = NetworkSender(9001, IPAddress.Loopback) 
        let mutable serverName = "NOT SET"

        let rec start(serverName': string) = 
            async {
                //printfn "state: start"; 
                serverName <- serverName';
                return! waitingFor2Players()
            }    
        
        and waitingFor2Players() = 
            async {
                //printfn "state: waitingFor2Players"; 
                let! msg = ev.Receive();
                match msg with
                | RequestServers ipAddress -> nwSender <- NetworkSender(9001, ipAddress);
                                              do! nwSender.Send(Server(GameServer(serverName, getOwnIpAddress)));
                                              return! waitingFor2Players();   
                | JoinGame ipAddress ->  nwSenderPlayer1 <- NetworkSender(9001, ipAddress);
                                         do! nwSenderPlayer1.Send(YouJoinedTheGame(1));
                                         return! waitingFor1Player();
                | _        -> return! waitingFor2Players()
            }

        /// wait until two players are connected
        and waitingFor1Player() = 
            async {
                //printfn "state: waitingForPlayers"; 
                let! msg = ev.Receive();
                match msg with
                | RequestServers ipAddress -> nwSender <- NetworkSender(9001, ipAddress);
                                              do! nwSender.Send(Server(GameServer(serverName, getOwnIpAddress)));
                                              return! waitingFor1Player();   
                | JoinGame ipAddress ->  nwSenderPlayer1 <- NetworkSender(9001, ipAddress);
                                         do! nwSenderPlayer1.Send(YouJoinedTheGame(2));
                                         return! startGame();
                | _         -> return! waitingFor1Player()
            }

        ///TODO: is this state needed? leave it for now incase WPF needs smthg special 
        and startGame() = 
            async {                
                return! waitFor2Inputs( 
                    GameState(
                        Ball(Vector(0.0f, 0.0f), Vector(-1.0f, 1.0f)), //Ball
                        PlayerData(Vector(-10.0f, 0.0f), 0), //Player 1
                        PlayerData(Vector(10.0f, 0.0f), 0) //Player 2
                        )
                    )
                }

        and waitFor2Inputs(state: GameState) = 
            async {  
                let! msg = ev.Receive();
                match msg with
                | PlayerInput (playerId, Escape) ->
                    return! leaving()
                | PlayerInput (playerId, key) -> 
                    let updatedState = GameEngine.calculateState(state.Ball, state.Player1, state.Player2, key, playerId)
                    return! waitFor1Inputs(updatedState);
                |_ -> return! waitFor2Inputs(state: GameState)

            }

        and waitFor1Inputs(state: GameState) = 
            async {                
                let! msg = ev.Receive();
                match msg with
                | PlayerInput (playerId, Escape) ->
                    return! leaving()
                | PlayerInput (playerId, key) -> 
                    let updatedState = GameEngine.calculateState(state.Ball, state.Player1, state.Player2, key, playerId)
                    return! sendGameStateUpdate(updatedState);
                |_ ->return! waitFor1Inputs(state);
            }

        ///sends a GameState to connected players
        and sendGameStateUpdate(state: GameState) = 
            async {            
                printfn "GameState: %A" (state)
                do! nwSenderPlayer1.Send(GameStateUpdate(state));
                do! nwSenderPlayer2.Send(GameStateUpdate(state));
                return! waitFor2Inputs(state)
                }
                
        //when an ESC is pressed -> server dies
        and leaving() =
              async {            
                do! nwSenderPlayer1.Send(GameDone);
                do! nwSenderPlayer2.Send(GameDone);
                //server dies
            }      

        /// call "Start" to launch a new Server 
        [<EntryPoint>]
        let main argv = 
            //TODO: fix this
            let serverName = argv.[0]
            Async.StartImmediate (start(serverName))                                   
            0 // return an integer exit code
