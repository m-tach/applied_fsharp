namespace Server

open System
open System.Net

open SharedTypes.NetworkStuff
open SharedTypes.SharedTypes

/// Automaton for the Server, hosting a ping-pong game
module ServerStuff = 

    type ServerStateMachine(serverName: string) =
        let ev = AsyncEventQueue<SharedTypes.SharedTypes.Message>()
        let messagesReceiver = NetworkReceiver(9001)
        let sender = NetworkSender(9001)
        do
            //receiver is only used to put messages in the event queue
            messagesReceiver.StartListening();
            messagesReceiver.ReceiveMessageEvent.Add(ev.Post)

        member public this.ServerName = serverName

        (*
            #############################
            ####### State machine #######            
            #############################

            Start
              |
              |            /------\
              V            V      | RequestServers
            WaitingFor2Players ---/
              |
              | JoinGame   /------\
              V            V      | RequestServers
            WaitingFor1Player ---/
              |
              | JoinGame
              V
            StartGame 
              |
              |       /-----------\
              V       V           |    PlayerInput Escape
            WaitFor2Inputs -------+---------------\
              |                   |                \
              | PlayerInput       |                 -----------> Leaving
              V                   |                /
            WaitFor1Input --------+---------------/
              |                   |    PlayerInput Escape
              | PlayerInput       |
              V                   |
            SendGameStateUpdate --/                      
        *)

        member public this.Start() = 
            async {
                return! this.WaitingFor2Players()
            }    
        
        member public this.WaitingFor2Players() = 
            async {
                let! msg = ev.Receive();
                match msg with
                | RequestServers ipAddress -> do! sender.Send(Server(GameServer(serverName, getOwnIpAddress)), ipAddress);
                                              return! this.WaitingFor2Players();   
                | JoinGame ipAddress ->  do! sender.Send(YouJoinedTheGame(1, getOwnIpAddress), ipAddress);
                                         return! this.WaitingFor1Player(ipAddress);
                | _        -> return! this.WaitingFor2Players()
            }

        /// wait until two players are connected
        member public this.WaitingFor1Player(player1Address: IPAddress) = 
            async {
                let! msg = ev.Receive();
                match msg with
                | RequestServers ipAddress -> do! sender.Send(Server(GameServer(serverName, getOwnIpAddress)), ipAddress);
                                              return! this.WaitingFor1Player(player1Address);   
                | JoinGame ipAddress ->  do! sender.Send(YouJoinedTheGame(2, getOwnIpAddress), ipAddress);
                                         return! this.StartGame(player1Address, ipAddress);
                | _         -> return! this.WaitingFor1Player(player1Address)
            }

        member public this.StartGame(player1Address: IPAddress, player2Address: IPAddress) = 
            async {                
                return! this.WaitFor2Inputs(player1Address, player2Address,
                    GameState(
                        Ball(Vector(0.0f, 0.0f), Vector(-1.0f, 1.0f)), //Ball
                        PlayerData(Vector(-10.0f, 0.0f), 0), //Player 1
                        PlayerData(Vector(10.0f, 0.0f), 0) //Player 2
                        )
                    )
                }

        member public this.WaitFor2Inputs(player1Address: IPAddress, player2Address: IPAddress, state: GameState) = 
            async {  
                let! msg = ev.Receive();
                match msg with
                | PlayerInput (_, Escape) -> return! this.Leaving(player1Address, player2Address)
                | PlayerInput (playerId, key) -> 
                    let updatedState = GameEngine.calculateState(state.Ball, state.Player1, state.Player2, key, playerId)
                    return! this.WaitFor1Input(player1Address, player2Address, updatedState);
                | _ -> return! this.WaitFor2Inputs(player1Address, player2Address, state)

            }

        member public this.WaitFor1Input(player1Address: IPAddress, player2Address: IPAddress, state: GameState) = 
            async {                
                let! msg = ev.Receive();
                match msg with
                | PlayerInput (_, Escape) -> return! this.Leaving(player1Address, player2Address)
                | PlayerInput (playerId, key) -> 
                    let updatedState = GameEngine.calculateState(state.Ball, state.Player1, state.Player2, key, playerId)
                    return! this.SendGameStateUpdate(player1Address, player2Address, updatedState);
                | _ -> return! this.WaitFor1Input(player1Address, player2Address, state);
            }

        ///sends a GameState to connected players
        member public this.SendGameStateUpdate(player1Address: IPAddress, player2Address: IPAddress, state: GameState) = 
            async {            
                do! sender.Send(GameStateUpdate(state), player1Address);
                do! sender.Send(GameStateUpdate(state), player2Address);
                return! this.WaitFor2Inputs(player1Address, player2Address, state)
                }
                
        //when an ESC is pressed -> server dies
        member public this.Leaving(player1Address: IPAddress, player2Address: IPAddress) =
              async {            
                do! sender.Send(GameDone, player1Address);
                do! sender.Send(GameDone, player2Address);
                //server dies
            }      

    /// call "Start" to launch a new Server 
    [<EntryPoint>]
    let main argv = 
        let serverName = argv.[0]
        let stateMachine = ServerStateMachine(serverName)
        Async.StartImmediate (stateMachine.Start())
        0 // return an integer exit code
