namespace Server
// Learn more about F# at http://fsharp.org

    open System
    open System.Threading

    open SharedTypes.SharedTypes

    /// Given an input, returns a new GameState
    module GameEngine =
        ///used for bouncing off walls
        let ballRadius = 1;

        ///Move player one up
        let moveUp pos = (fst pos , (snd pos - 1.0))
        
        ///Move player one down
        let moveDown pos = (fst pos , (snd pos + 1.0))

        ///Move ball 
        let moveBall (ball':Ball) : Ball = 
            let rx = fst (fst ball')
            let ry = snd (fst ball')
            let vx = fst (snd ball')
            let vy = snd (snd ball')
            ((rx + vx, ry + vy),(vx, vy))

        let calculateState playerData =   
            playerData 



    /// Automaton for the Server, hosting a ping-pong game
    module StateMachine = 
    //TODO: for now, event queue stores strings
        let ev = AsyncEventQueue<String>()

        let rec start() = 
            async {
                printfn "state: start"; 
                return! waitingForPlayers()
                }    

        /// wait until two players are connected
        and waitingForPlayers() = 
            async {
                printfn "state: waitingForPlayers"; 
                let! msg = ev.Receive();
                match msg with
                 | "Two players have now joined"  -> return! playGame()
                 | _         -> failwith("waitingForPlayers: unexpected message")
                }

        ///TODO: is this state needed? leave it for now incase WPF needs smthg special 
        and playGame() = 
            async {

                printfn "state: playGame";                 
                return! sendNewState(GameEngine.moveBall( (0.0, 0.0), (-0.015, 0.015)))
                }

        ///sends a GameState to connected players
        and sendNewState(state: Ball) = 
            async {
                printfn "state: sendNewState";
                printfn "Ball position: %A" (fst state)
                return! waitForClientInput(state)
                }

        ///updates PlayerData for corresponding player
        and waitForClientInput(state: Ball) = 
            async {
                printfn "state: waitForClientInput"; 
                let! msg = ev.Receive();
                match msg with
                 | "Up" -> 
                     return! sendNewState(GameEngine.moveBall(state))
                 | "Down" -> 
                     return! sendNewState(GameEngine.moveBall(state))
                 | _         -> failwith("waitForClientInput: unexpected message")
                }    

        /// call "Start" to launch a new Server 
        [<EntryPoint>]
        let main argv =
            printfn "Main"
            Async.StartImmediate (start())
            ev.Post "Two players have now joined"
            Thread.Sleep(500)
            ev.Post "Up"
            Thread.Sleep(500)
            ev.Post "Up"
            Thread.Sleep(500)
            ev.Post "Up"
            0 // return an integer exit code
