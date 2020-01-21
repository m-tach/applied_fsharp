namespace Server
// Learn more about F# at http://fsharp.org

    open System
    open System.Threading

    open SharedTypes.SharedTypes

    /// Given an input key, calculates a new GameState
    module GameEngine =
        let third (_, _, c) = c

        ///used for bouncing off walls
        let ballRadius = 1.0;
        //used for hitting ball
        let playerPaddleSize = (1, 3); //paddle is 1 wide and 3 long 

        ///Move player one up
        let moveUp (pos, score) : PlayerData = if snd pos < 10.0 then ((fst pos , (snd pos + 1.0)), score) else ((fst pos, snd pos), score)
        
        ///Move player one down
        let moveDown (pos, score) : PlayerData = if snd pos > -10.0 then ((fst pos , (snd pos - 1.0)), score) else ((fst pos, snd pos), score)

        let incrementScore player: PlayerData = (fst player, (snd player + 1) )

        let restartGame (player1, player2) : GameState = 
            printfn "Ball has left the field : restarting game"
            (
                ((0.0, 0.0), (-1.0, 1.0)), //Ball
                ((-10.0, 0.0), snd player1), //Player 1
                ((10.0, 0.0), snd player2) //Player 2
            ) 

        ///Move ball; change direction if hitting top edge; bottom edge; player paddle
        ///Change score if gone through edge and restart from center
        let moveBall (ball':Ball, player1:PlayerData, player2:PlayerData) : Ball = 
            let rx = fst (fst ball')
            let ry = snd (fst ball')

            let vy = if (ry + snd (snd ball') + ballRadius > 10.0) then -(snd (snd ball'))
                     elif (ry + snd (snd ball') + ballRadius < -10.0) then -(snd (snd ball'))
                     else snd (snd ball')            
            let vx = if rx +  fst (snd ball') < -9.0 then //is ball at left edge
                        if (((ry + vy) > (fst (fst player1) + 1.5)) ||             //is ball hitting player1
                            ((ry + vy) < fst (fst player1) - 1.5)) 
                        then 
                            restartGame (player1, incrementScore player2)
                            1.0
                        else //player 1 has hit ball
                            -(fst (snd ball'))
                     elif rx +  fst (snd ball') > 9.0 then //is ball at right edge
                        if (((ry + vy) > fst (fst player2) + 1.5) ||             //is ball hitting player2
                            ((ry + vy) < fst (fst player2) - 1.5)) 
                        then 
                            restartGame (incrementScore player1, player2)
                            1.0
                        else //player 2 has hit ball
                            -(fst (snd ball'))
                     else
                        fst (snd ball')
            ((rx + vx, ry + vy),(vx, vy))

        ///Derive new state from old state + key press command
        let calculateState ((ball, player1, player2), command:Input, player: String) : GameState =
            let (p1, p2 ) = if (player = "P1") then 
                                match command with
                                | Up -> (moveUp player1, player2)
                                | Down -> (moveDown  player1, player2)
                            elif (player = "P2") then 
                                match command with
                                     | Up -> (player1, moveUp player2)
                                     | Down -> (player1, moveDown  player2)              
                            else
                                (player1, player2)    
            
            let newBall = moveBall (ball, player1, player2)

            (newBall, p1, p2)



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
                return! sendNewState( 
                    ((0.0, 0.0), (-1.0, 1.0)), //Ball
                    ((-10.0, 0.0), 0), //Player 1
                    ((10.0, 0.0), 0) //Player 2
                    )
                }

        ///sends a GameState to connected players
        and sendNewState(state: GameState) = 
            async {
                printfn "state: sendNewState";
                printfn "GameState: %A" (state)
                //TODO: send gameState via Comm library
                return! waitForClientInput(state)
                }

        ///updates PlayerData for corresponding player
        and waitForClientInput(state: GameState) = 
            async {
                printfn "state: waitForClientInput"; 
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
            printfn "Main"
            Async.StartImmediate (start())
            ev.Post "Two players have now joined"
            for i in 1 .. 200 do
                Thread.Sleep(500)
                ev.Post "Up;P1"
                Thread.Sleep(500)
                ev.Post "Up;P2"
                Thread.Sleep(500)
                ev.Post "Up;P1"
            0 // return an integer exit code
