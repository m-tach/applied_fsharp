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
        [<Literal>]
        let private BALL_SPEED_X = -0.07f
        [<Literal>]
        let private BALL_SPEED_Y = 0.075f

        // Configs - player
        [<Literal>]
        /// distance player moves on one press on Up/Down key
        let private MOVE_DISTANCE_PLAYER = 0.3f
        [<Literal>]
        let private PADDLE_LENGHT = 4.0f
        [<Literal>]
        let private PADDLE_WIDTH = 1.0f
       
        let public DEFAULT_GAME_STATE =
            GameState(
                Ball(Vector(0.0f, 0.0f), Vector(BALL_SPEED_X, BALL_SPEED_Y)), //Ball
                PlayerData(Vector(-10.0f, 0.0f), 0), //Player 1
                PlayerData(Vector(10.0f, 0.0f), 0) //Player 2
                )
        

        ///Move player one up
        let private moveUp (player: PlayerData) : PlayerData = 
            PlayerData(Vector(player.Position.X , min (Y_MAX - (PADDLE_LENGHT / 2.0f)) (player.Position.Y + MOVE_DISTANCE_PLAYER)), player.Score)
        
        ///Move player one down
        let private moveDown (player: PlayerData) : PlayerData = 
            PlayerData(Vector(player.Position.X , max (Y_MIN + (PADDLE_LENGHT / 2.0f)) (player.Position.Y - MOVE_DISTANCE_PLAYER)), player.Score)
        
        ///Increment score of player
        let private incrementScore (player: PlayerData) = PlayerData(player.Position, (player.Score + 1) )

        ///Restart game - reset positions of ball and players
        let private restartGame (player1: PlayerData, player2: PlayerData) : GameState = 
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
        let private checkBounds (ball:Ball, player1:PlayerData, player2:PlayerData) : GameState =
            match ball.BallPosition.X with
            | bx when bx < X_MIN -> restartGame (player1, incrementScore player2)
            | bx when bx > X_MAX -> restartGame (incrementScore player1, player2)
            | _ -> GameState(ball,player1, player2)                        

        ///Move ball; change direction if hitting top edge; bottom edge; player paddle
        let private moveBall (ball:Ball, player1:PlayerData, player2:PlayerData) : Ball = 
            let rx = ball.BallPosition.X
            let ry = ball.BallPosition.Y
            
            // change direction when hitting top / bottom
            let vy = if (ry + ball.BallDirection.Y + BALL_RADIUS > Y_MAX) ||
                        (ry + ball.BallDirection.Y - BALL_RADIUS < Y_MIN)
                     then -ball.BallDirection.Y
                     else  ball.BallDirection.Y

            let player = if ball.BallDirection.X < 0.0f then player1 else player2
            let willBallBeOutsidePlayArea = 
                rx + ball.BallDirection.X < (X_MIN + BALL_RADIUS) ||
                rx + ball.BallDirection.X > (X_MAX - BALL_RADIUS)
            let ballWithinPaddleYBound =
                (ry + vy < player.Position.Y + PADDLE_LENGHT/2.0f) &&
                (ry + vy > player.Position.Y - PADDLE_LENGHT/2.0f)                     
            let vx = if willBallBeOutsidePlayArea && ballWithinPaddleYBound 
                     then -ball.BallDirection.X
                     else  ball.BallDirection.X 
            Ball(Vector(rx + vx, ry + vy), Vector(vx, vy))

        let public calculateState (ball:Ball, player1:PlayerData, player2:PlayerData, command:Input, player: int) : GameState =
            let (p1, p2 ) = match (player, command) with
                            | (1, Up)   -> (moveUp    player1, player2)
                            | (1, Down) -> (moveDown  player1, player2)
                            | (2, Up)   -> (player1, moveUp    player2)
                            | (2, Down) -> (player1, moveDown  player2)   
                            | (p, i) -> failwith (String.Format("Player {0} with input {1} is invalid to update the state", p, i))
            let newBall = moveBall (ball, p1, p2)
            checkBounds(newBall, p1, p2)