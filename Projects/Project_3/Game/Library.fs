namespace Game
    open System
    open SharedTypes.SharedTypes

   /// Given an input key, calculates a new GameState
    module GameEngine =
        // Configs - field       
        [<Literal>]
        let private X_MAX = 10.0f
        
        [<Literal>]
        let private X_MIN = -10.0f
                
        [<Literal>]
        let private Y_MAX = 10.0f
        
        [<Literal>]
        let private Y_MIN = -10.0f
        
        [<Literal>]
        let private X_MIDDLE = 0.0f
        
        [<Literal>]
        let private Y_MIDDLE = 0.0f

        // Configs - ball
        [<Literal>]
        let private BALL_RADIUS = 1.0f
        let private BALL_SPEED_X = -1.0f
        let private BALL_SPEED_Y = 1.0f

        // Configs - player
        [<Literal>]
        /// distance player moves on one press on Up/Down key
        let private MOVE_DISTANCE_PLAYER = 1.0f
        [<Literal>]
        let private PADDLE_LENGHT = 3.0f
        [<Literal>]
        let private PADDLE_WIDTH = 1.0f
        

        ///Move player one up
        let moveUp (player: PlayerData) : PlayerData = if player.Position.Y < Y_MAX 
                                                       then PlayerData(Vector(player.Position.X , (player.Position.Y + MOVE_DISTANCE_PLAYER)), player.Score) 
                                                       else player
        
        ///Move player one down
        let moveDown (player: PlayerData) : PlayerData = if player.Position.Y > Y_MIN 
                                                         then PlayerData(Vector(player.Position.X , (player.Position.Y - MOVE_DISTANCE_PLAYER)), player.Score)
                                                         else player
        ///Increment score of player
        let incrementScore (player: PlayerData) = PlayerData(player.Position, (player.Score + 1) )

        ///Restart game - reset positions of ball and players
        let restartGame (player1: PlayerData, player2: PlayerData) : GameState = 
            printfn "Ball has left the field : restarting game"
            printfn "Player1 score: %d " player1.Score
            printfn "Player2 score: %d " player2.Score
            GameState(
                Ball(Vector(X_MIDDLE, Y_MIDDLE), Vector(BALL_SPEED_X, BALL_SPEED_Y)), //Ball
                PlayerData(Vector(X_MIN, Y_MIDDLE), player1.Score), //Player 1
                PlayerData(Vector(X_MAX, Y_MIDDLE), player2.Score) //Player 2
            ) 

        /// Check if ball will be hit or missed by a player the next time it moves. 
        /// If missed - increment score and restart positions
        let checkBounds (ball':Ball, player1:PlayerData, player2:PlayerData) : GameState =
            let rx = ball'.BallPosition.X
            let ry = ball'.BallPosition.Y
            let vy = ball'.BallDirection.Y//direction y

            if rx + ball'.BallDirection.X < (X_MIN + BALL_RADIUS) then //is ball at left edge
                if (((ry + vy) > player1.Position.Y + PADDLE_LENGHT/2.0f ) ||             
                    ((ry + vy) < player1.Position.Y - PADDLE_LENGHT/2.0f )) 
                then 
                    printfn "Player1 missed ball"
                    restartGame (player1, incrementScore player2)
                else 
                    printfn "Player1 hit ball"
                    GameState(ball',player1, player2)    

            else if rx + ball'.BallDirection.X > (X_MAX - BALL_RADIUS) then //is ball at right edge
                if (((ry + vy) > player2.Position.Y + PADDLE_LENGHT/2.0f ) ||             //is ball hitting player1
                    ((ry + vy) < player2.Position.Y - PADDLE_LENGHT/2.0f )) 
                then 
                    printfn "Player2 missed ball"
                    restartGame (incrementScore player1, player2)
                else 
                    printfn "Player2 hit ball"
                    GameState(ball', player1, player2)

            else
                GameState(ball', player1, player2)                            

        ///Move ball; change direction if hitting top edge; bottom edge; player paddle
        let moveBall (ball':Ball, player1:PlayerData, player2:PlayerData) : Ball = 
            let rx = ball'.BallPosition.X
            let ry = ball'.BallPosition.Y
            
            // change direction when hitting top / bottom
            let vy = if (ry + ball'.BallDirection.Y + BALL_RADIUS > Y_MAX) then -(ball'.BallDirection.Y)
                     elif (ry + ball'.BallDirection.Y + BALL_RADIUS < Y_MIN) then -(ball'.BallDirection.Y)
                     else ball'.BallDirection.Y
                                 
            let vx = if rx + ball'.BallDirection.X < (X_MIN + BALL_RADIUS) then //is ball at left edge
                        if (((ry + vy) < player1.Position.Y + PADDLE_LENGHT/2.0f) ||             //is ball hitting player1
                            ((ry + vy) > player1.Position.Y - PADDLE_LENGHT/2.0f)) 
                            then -ball'.BallDirection.X
                        else 
                            1.0f                                                
                     elif rx +  ball'.BallDirection.X > (X_MAX - BALL_RADIUS) then //is ball at right edge
                        if (((ry + vy) < player2.Position.Y + PADDLE_LENGHT/2.0f) ||             //is ball hitting player2
                            ((ry + vy) > player2.Position.Y - PADDLE_LENGHT/2.0f)) 
                            then -ball'.BallDirection.X
                        else
                            1.0f                                               
                     else
                        ball'.BallDirection.X
            Ball(Vector(rx + vx, ry + vy), Vector(vx, vy))

        /// Derive new state from old state + key press command
        /// Flow:
        /// 1. Move player
        /// 2. Check if ball's current trajectory will be missed by the paddle in this turn
        ///     - if yes -> restart game
        ///     - if no -> move ball 
        let calculateState (ball:Ball, player1:PlayerData, player2:PlayerData, command:Input, player: int) : GameState =
            let (p1, p2 ) = if (player = 1) then 
                                match command with
                                | Up -> (moveUp player1, player2)
                                | Down -> (moveDown  player1, player2)
                                | i -> failwith (String.Format("unexpected input: {0}", i))
                            elif (player = 2) then 
                                match command with
                                     | Up -> (player1, moveUp player2)
                                     | Down -> (player1, moveDown  player2)   
                                     | i -> failwith (String.Format("unexpected input: {0}", i))           
                            else
                                failwith (String.Format("unexpected player: {0}", player))    
            let gState = checkBounds(ball, p1, p2)
            let newBall = moveBall (gState.Ball, gState.Player1, gState.Player2)

            GameState(newBall, gState.Player1, gState.Player2)