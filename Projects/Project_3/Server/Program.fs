namespace Server
// Learn more about F# at http://fsharp.org

    open System
    open System.Net
    open System.Threading

    open SharedTypes.NetworkStuff
    open SharedTypes.SharedTypes 
    open GameEngine

    /// Automaton for the Server, hosting a ping-pong game
    module StateMachine = 
    //TODO: for now, event queue stores strings
        let ev = AsyncEventQueue<String>()

        let rec start() = 
            async {
                //printfn "state: start"; 
                return! waitingForPlayers()
                }    

        /// wait until two players are connected
        and waitingForPlayers() = 
            async {
                //printfn "state: waitingForPlayers"; 
                let nwRec = NetworkReceiver(9001)
                nwRec.StartListening();
                // TODO: handle different types of messages
                nwRec.ReceiveMessageEvent.Add( 
                    fun x -> ev.Post(System.Text.Encoding.UTF8.GetString(x)) 
                    )
                let! msg = ev.Receive();
                match msg with
                 | "Two players have now joined"  -> return! playGame()
                 | _         -> failwith("waitingForPlayers: unexpected message")
                }

        ///TODO: is this state needed? leave it for now incase WPF needs smthg special 
        and playGame() = 
            async {
                //printfn "state: playGame";                 
                return! sendNewState( 
                    GameState(
                        Ball(Vector(0.0f, 0.0f), Vector(-1.0f, 1.0f)), //Ball
                        PlayerData(Vector(-10.0f, 0.0f), 0), //Player 1
                        PlayerData(Vector(10.0f, 0.0f), 0) //Player 2
                        )
                    )
                }

        ///sends a GameState to connected players
        and sendNewState(state: GameState) = 
            async {
                //printfn "state: sendNewState";
                printfn "GameState: %A" (state)
                //TODO: send gameState via Comm library
                return! waitForClientInput(state)
                }

        ///updates PlayerData for corresponding player
        and waitForClientInput(state: GameState) = 
            async {
                //printfn "state: waitForClientInput"; 
                let! msg = ev.Receive();
                //TODO: replace strings with actual events (how to identify which player is which)
                match msg with
                 | "Up;P1" -> 
                     return! sendNewState(GameEngine.calculateState(state.Ball, state.Player1, state.Player2, Up, "P1" ))
                 | "Up;P2" -> 
                     return! sendNewState(GameEngine.calculateState(state.Ball, state.Player1, state.Player2, Up, "P2" ))
                 | "Down;P1" -> 
                     return! sendNewState(GameEngine.calculateState(state.Ball, state.Player1, state.Player2, Down, "P1"))
                 | "Down;P2" -> 
                     return! sendNewState(GameEngine.calculateState(state.Ball, state.Player1, state.Player2, Down, "P2"))
                 | _         -> failwith("waitForClientInput: unexpected message")
                }    

        /// call "Start" to launch a new Server 
        [<EntryPoint>]
        let main argv =
            //Testing ball movement in game engine
            Async.StartImmediate (start())
            ev.Post "Two players have now joined"
            for i in 1 .. 20 do
                Thread.Sleep(500)
                ev.Post "Up;P1"
                Thread.Sleep(500)
                ev.Post "Up;P2"
                Thread.Sleep(500)
                ev.Post "Down;P1"
                Thread.Sleep(500)
                ev.Post "Up;P1"

            //Testing broadcasting
            let receiver = NetworkReceiver(9001)
            let sender = NetworkSender(9001, IPAddress.Loopback)

            receiver.ReceiveMessageEvent.Add(fun x -> printfn "%s" (System.Text.Encoding.UTF8.GetString(x)))

            receiver.StartListening()

            Async.RunSynchronously (Broadcast(System.Text.Encoding.UTF8.GetBytes("the thing!"), 9001))
            Async.RunSynchronously (Broadcast(System.Text.Encoding.UTF8.GetBytes("the thing!"), 9001))
            Async.RunSynchronously (Broadcast(System.Text.Encoding.UTF8.GetBytes("the thing!"), 9001))
            Async.RunSynchronously (Broadcast(System.Text.Encoding.UTF8.GetBytes("the thing!"), 9001))


            Async.RunSynchronously (sender.Send(System.Text.Encoding.UTF8.GetBytes("the taahing!"))) |> ignore
            Async.RunSynchronously (sender.Send(System.Text.Encoding.UTF8.GetBytes("the taahing!"))) |> ignore
            Async.RunSynchronously (sender.Send(System.Text.Encoding.UTF8.GetBytes("the taahing!"))) |> ignore
            Async.RunSynchronously (sender.Send(System.Text.Encoding.UTF8.GetBytes("the taahing!"))) |> ignore

            Async.RunSynchronously (Async.Sleep(3000))            
            0 // return an integer exit code
