namespace Server
// Learn more about F# at http://fsharp.org

    open System
    open SharedTypes.SharedTypes

    module StateMachine = 
    //TODO: for now, event queue stores strings
        let ev = AsyncEventQueue<String>()

        /// Automaton for the Server, hosting a ping-pong game
        let rec start() = 
            async {printfn "state: start"; return! waitingForPlayers()}    

        /// wait until two players are connected
        and waitingForPlayers() = 
            async {
                printfn "state: waitingForPlayers"; 
                let! msg = ev.Receive();
                match msg with
                 | "Start"  -> return! playGame()
                 | _         -> failwith("waitingForPlayers: unexpected message")
                }

        ///TODO: is this state needed? 
        and playGame() = 
            async {
                printfn "playGame"; return! sendNewState()
                }

        and sendNewState() = 
            async {
                printfn "sendNewState"; return! waitForClientInput()
                }   

        and waitForClientInput() = 
            async {
                printfn "waitForClientInput"; return! sendNewState()
                }    

        /// call "Start" to launch a new Server 
        [<EntryPoint>]
        let main argv =
            printfn "Main"
            Async.StartImmediate (start())
            ev.Post "Start"

            0 // return an integer exit code
