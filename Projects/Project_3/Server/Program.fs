namespace Server

open System
open System.Net

open SharedTypes.NetworkStuff
open SharedTypes.SharedTypes
open SharedTypes.Constants
open Game
open ComputerOpponent
open System.Threading


/// Automaton for the Server, hosting a ping-pong game
module ServerStuff = 

    type ServerStateMachine(serverName: string, againstComputer : bool) =
        let ev = AsyncEventQueue<SharedTypes.SharedTypes.Message>()
        let messagesReceiver = NetworkReceiver(SERVER_PORT)
        let sender = NetworkSender(CLIENT_PORT)
        let computerSender = NetworkSender(9003)
        do
            printfn "Server name: %s" serverName
            printfn "Use bot: %s" (againstComputer.ToString())
            //receiver is only used to put messages in the event queue
            messagesReceiver.StartListening();
            messagesReceiver.ReceiveMessageEvent.Add(ev.Post)
            if againstComputer
            then Async.StartImmediate (async {
                let compOpponent = ComputerOpponentStateMachine();
                do! compOpponent.JoinGame();                       
            })

        member public this.ServerName = serverName
        member public this.AgainstComputer = againstComputer

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
                printfn "state: Start"; 

                return! this.WaitingFor2Players()
            }    
        
        member public this.WaitingFor2Players() = 
            async {
                printfn "state: WaitingFor2Players"; 

                let! msg = ev.Receive();
                printfn "parsing msg: %A" msg; 
                match msg with
                | RequestServers ipAddress -> do! sender.Send(Server(GameServer(serverName, getOwnIpAddress)), ipAddress);
                                              return! this.WaitingFor2Players();   
                | JoinGame ipAddress 
                    when againstComputer && ipAddress.Equals(IPAddress.Loopback) ->
                    printfn "awjaowijfaowijdawoijf"                    
                    do! computerSender.Send(YouJoinedTheGame(1, getOwnIpAddress), ipAddress);
                    return! this.WaitingFor1Player(ipAddress);
                | JoinGame ipAddress ->  do! sender.Send(YouJoinedTheGame(1, getOwnIpAddress), ipAddress);
                                         return! this.WaitingFor1Player(ipAddress);
                | _        -> return! this.WaitingFor2Players()
            }

        /// wait until two players are connected
        member public this.WaitingFor1Player(player1Address: IPAddress) = 
            async {
                printfn "state: WaitingFor1Player"; 

                let! msg = ev.Receive();
                printfn "parsing msg: %A" msg; 

                match msg with
                | RequestServers ipAddress -> do! sender.Send(Server(GameServer(serverName, getOwnIpAddress)), ipAddress);
                                              return! this.WaitingFor1Player(player1Address);   
                | JoinGame ipAddress 
                    when againstComputer && ipAddress.Equals(IPAddress.Loopback) ->
                    do! computerSender.Send(YouJoinedTheGame(2, getOwnIpAddress), ipAddress);                     
                    return! this.StartGame(player1Address, ipAddress);
                | JoinGame ipAddress ->  do! sender.Send(YouJoinedTheGame(2, getOwnIpAddress), ipAddress);
                                         return! this.StartGame(player1Address, ipAddress);
                | _         -> return! this.WaitingFor1Player(player1Address)
            }

        member public this.StartGame(player1Address: IPAddress, player2Address: IPAddress) = 
            async {             
                printfn "state: StartGame"; 
                if (againstComputer) && player1Address.Equals(IPAddress.Loopback)
                then do! computerSender.Send(StartGame, player1Address)
                else do! sender.Send(StartGame, player1Address);
                
                if (againstComputer) && player2Address.Equals(IPAddress.Loopback)
                then do! computerSender.Send(StartGame, player2Address)
                else do! sender.Send(StartGame, player2Address);

                return! this.WaitFor2Inputs(player1Address, player2Address,
                    GameEngine.DEFAULT_GAME_STATE(0, 0)
                    )
                }

        member public this.WaitFor2Inputs(player1Address: IPAddress, player2Address: IPAddress, state: GameState) = 
            async {  
                printfn "state: WaitFor2Inputs"; 

                use cancel = new CancellationTokenSource(TimeSpan.FromSeconds(1.0))
                Async.StartWithContinuations(
                    async { return! ev.Receive() }, 
                    (fun x -> ev.Post(x)), 
                    (fun x -> Console.Error.WriteLine(x.ToString())), 
                    (fun _ -> ev.Post(GameDone)), 
                    cancel.Token)

                let! msg = ev.Receive();
                printfn "parsing msg: %A" msg;
                match msg with
                | PlayerInput (_, Escape) -> return! this.Leaving(player1Address, player2Address)
                | PlayerInput (playerId, key) -> 
                    let updatedState = GameEngine.calculateState(state.Ball, state.Player1, state.Player2, key, playerId)
                    return! this.WaitFor1Input(player1Address, player2Address, updatedState);
                | GameDone -> return! this.Leaving(player1Address, player2Address)              
                | _ -> return! this.WaitFor2Inputs(player1Address, player2Address, state)

            }

        member public this.WaitFor1Input(player1Address: IPAddress, player2Address: IPAddress, state: GameState) = 
            async {                
                printfn "state: WaitFor1Input"; 

                use cancel = new CancellationTokenSource(TimeSpan.FromSeconds(1.0))
                Async.StartWithContinuations(
                    async { return! ev.Receive() }, 
                    (fun x -> ev.Post(x)), 
                    (fun x -> Console.Error.WriteLine(x.ToString())), 
                    (fun _ -> ev.Post(GameDone)), 
                    cancel.Token)            

                let! msg = ev.Receive();
                printfn "parsing msg: %A" msg;
                match msg with
                | PlayerInput (_, Escape) -> return! this.Leaving(player1Address, player2Address)
                | PlayerInput (playerId, key) -> 
                    let updatedState = GameEngine.calculateState(state.Ball, state.Player1, state.Player2, key, playerId)
                    return! this.SendGameStateUpdate(player1Address, player2Address, updatedState);
                | GameDone -> return! this.Leaving(player1Address, player2Address)                  
                | _ -> return! this.WaitFor1Input(player1Address, player2Address, state);
            }

        ///sends a GameState to connected players
        member public this.SendGameStateUpdate(player1Address: IPAddress, player2Address: IPAddress, state: GameState) = 
            async {       
                printfn "state: SendGameStateUpdate"; 

                if againstComputer && player1Address.Equals(IPAddress.Loopback)
                then do! computerSender.Send(GameStateUpdate(state), player1Address)
                else do! sender.Send(GameStateUpdate(state), player1Address);

                if againstComputer && player2Address.Equals(IPAddress.Loopback)
                then do! computerSender.Send(GameStateUpdate(state), player2Address);
                else do! sender.Send(GameStateUpdate(state), player2Address);

                return! this.WaitFor2Inputs(player1Address, player2Address, state)
                }
                
        //when an ESC is pressed -> server dies
        member public this.Leaving(player1Address: IPAddress, player2Address: IPAddress) =
              async {     
                printfn "state: Leaving"; 
                if againstComputer  && player1Address.Equals(IPAddress.Loopback)
                then do! computerSender.Send(GameDone, player1Address)
                else do! sender.Send(GameDone, player1Address);

                if againstComputer  && player2Address.Equals(IPAddress.Loopback)
                then do! computerSender.Send(GameDone, player2Address)
                else do! sender.Send(GameDone, player2Address);
                //server dies
            }      

    /// call "Start" to launch a new Server 
    [<EntryPoint>]
    let main argv = 
        try             
            printfn "Server has just started"; 
            let serverName = argv.[0]
            let againstComputer = argv.[1]
            printfn "argument 1: %s" serverName
            printfn "argument 2: %s" againstComputer
            let stateMachine = ServerStateMachine(serverName, bool.Parse(againstComputer))
            Async.RunSynchronously (stateMachine.Start())
            printfn "Server is done"; 
        with 
        |e -> printfn "Exception thrown %A" e        
        0 // return an integer exit code
