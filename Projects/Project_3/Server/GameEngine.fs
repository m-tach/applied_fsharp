namespace Server
    open System
    open SharedTypes.SharedTypes

   /// Given an input key, calculates a new GameState
    module GameEngine =
        // Configs - field       
        [<Literal>]
        let private X_MAX = 10.0
        
        [<Literal>]
        let private X_MIN = -10.0
                
        [<Literal>]
        let private Y_MAX = 10.0
        
        [<Literal>]
        let private Y_MIN = -10.0
        
        [<Literal>]
        let private X_MIDDLE = 0.0
        
        [<Literal>]
        let private Y_MIDDLE = 0.0

        // Configs - ball
        [<Literal>]
        let private BALL_RADIUS = 1.0
        let private BALL_SPEED_X = -1.0
        let private BALL_SPEED_Y = 1.0

        // Configs - player
        [<Literal>]
        /// distance player moves on one press on Up/Down key
        let private MOVE_DISTANCE_PLAYER = 1.0
        [<Literal>]
        let private PADDLE_LENGHT = 3.0
        [<Literal>]
        let private PADDLE_WIDTH = 1.0
        

        ///Move player one up
        let moveUp (pos, score) : PlayerData = if snd pos < Y_MAX 
                                               then ((fst pos , (snd pos + MOVE_DISTANCE_PLAYER)), score) 
                                               else ((fst pos, snd pos), score)
        
        ///Move player one down
        let moveDown (pos, score) : PlayerData = if snd pos > Y_MIN 
                                                 then ((fst pos , (snd pos - MOVE_DISTANCE_PLAYER)), score)
                                                 else ((fst pos, snd pos), score)
        ///Increment score of player
        let incrementScore player: PlayerData = (fst player, (snd player + 1) )

        ///Restart game - reset positions of ball and players
        let restartGame (player1, player2) : GameState = 
            printfn "Ball has left the field : restarting game"
            printfn "Player1 score: %d " (snd player1)
            printfn "Player2 score: %d " (snd player2)
            (
                ((X_MIDDLE, Y_MIDDLE), (BALL_SPEED_X, BALL_SPEED_Y)), //Ball
                ((X_MIN, Y_MIDDLE), snd player1), //Player 1
                ((X_MAX, Y_MIDDLE), snd player2) //Player 2
            ) 

        /// Check if ball will be hit or missed by a player the next time it moves. 
        /// If missed - increment score and restart positions
        let checkBounds (ball':Ball, player1:PlayerData, player2:PlayerData) : GameState =
            let rx = fst (fst ball')//posirion x
            let ry = snd (fst ball')//position y
            let vy = snd (snd ball')//direction y

            if rx +  fst (snd ball') < (X_MIN + BALL_RADIUS) then //is ball at left edge
                if (((ry + vy) > (fst (fst player1) + PADDLE_LENGHT/2.0 )) ||             
                    ((ry + vy) < fst (fst player1) - PADDLE_LENGHT/2.0 )) 
                then 
                    printfn "Player1 missed ball"
                    restartGame (player1, incrementScore player2)
                else 
                    printfn "Player1 hit ball"
                    (ball',player1, player2)    

            else if rx +  fst (snd ball') > (X_MAX - BALL_RADIUS) then //is ball at right edge
                if (((ry + vy) > (fst (fst player2) + PADDLE_LENGHT/2.0 )) ||             //is ball hitting player1
                    ((ry + vy) < fst (fst player2) - PADDLE_LENGHT/2.0 )) 
                then 
                    printfn "Player2 missed ball"
                    restartGame (incrementScore player1, player2)
                else 
                    printfn "Player2 hit ball"
                    (ball', player1, player2)

            else
                (ball', player1, player2)                            

        ///Move ball; change direction if hitting top edge; bottom edge; player paddle
        let moveBall (ball':Ball, player1:PlayerData, player2:PlayerData) : Ball = 
            let rx = fst (fst ball')
            let ry = snd (fst ball')
            
            // change direction when hitting top / bottom
            let vy = if (ry + snd (snd ball') + BALL_RADIUS > Y_MAX) then -(snd (snd ball'))
                     elif (ry + snd (snd ball') + BALL_RADIUS < Y_MIN) then -(snd (snd ball'))
                     else snd (snd ball')
                                 
            let vx = if rx +  fst (snd ball') < (X_MIN + BALL_RADIUS) then //is ball at left edge
                        if (((ry + vy) < (fst (fst player1) + PADDLE_LENGHT/2.0)) ||             //is ball hitting player1
                            ((ry + vy) > fst (fst player1) - PADDLE_LENGHT/2.0)) 
                            then -(fst (snd ball'))
                        else 
                            1.0                                                
                     elif rx +  fst (snd ball') > (X_MAX - BALL_RADIUS) then //is ball at right edge
                        if (((ry + vy) < fst (fst player2) + PADDLE_LENGHT/2.0) ||             //is ball hitting player2
                            ((ry + vy) > fst (fst player2) - PADDLE_LENGHT/2.0)) 
                            then -(fst (snd ball'))
                        else
                            1.0                                                
                     else
                        fst (snd ball')
            ((rx + vx, ry + vy),(vx, vy))

        /// Derive new state from old state + key press command
        /// Flow:
        /// 1. Move player
        /// 2. Check if ball's current trajectory will be missed by the paddle in this turn
        ///     - if yes -> restart game
        ///     - if no -> move ball 
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
            let (outside, p1', p2') = checkBounds(ball, p1, p2)
            let newBall = moveBall (outside, p1', p2')

            (newBall, p1', p2')