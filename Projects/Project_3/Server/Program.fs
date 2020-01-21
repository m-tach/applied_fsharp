namespace Server
// Learn more about F# at http://fsharp.org

    open System
    open System.Threading

    open SharedTypes.SharedTypes 

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
                    ((0.0, 0.0), (-1.0, 1.0)), //Ball
                    ((-10.0, 0.0), 0), //Player 1
                    ((10.0, 0.0), 0) //Player 2
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
                     return! sendNewState(GameEngine.calculateState(state, Up, "P1" ))
                 | "Up;P2" -> 
                     return! sendNewState(GameEngine.calculateState(state, Up, "P2" ))
                 | "Down;P1" -> 
                     return! sendNewState(GameEngine.calculateState(state, Down, "P1"))
                 | "Down;P2" -> 
                     return! sendNewState(GameEngine.calculateState(state, Down, "P2"))
                 | _         -> failwith("waitForClientInput: unexpected message")
                }    

        /// call "Start" to launch a new Server 
        [<EntryPoint>]
        let main argv =
            //printfn "Main"
            Async.StartImmediate (start())
            ev.Post "Two players have now joined"
            for i in 1 .. 200 do
                Thread.Sleep(500)
                ev.Post "Up;P1"
                Thread.Sleep(500)
                ev.Post "Up;P2"
                Thread.Sleep(500)
                ev.Post "Down;P1"
                Thread.Sleep(500)
                ev.Post "Up;P1"
            0 // return an integer exit code
