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
        let private BALL_SPEED_X = -0.1f
        [<Literal>]
        let private BALL_SPEED_Y = 0.1f
        [<Literal>]
        let private MAX_BOUNCE_ANGLE = 5.0f;

        // Configs - player
        [<Literal>]
        /// distance player moves on one press on Up/Down key
        let private MOVE_DISTANCE_PLAYER = 0.4f
        [<Literal>]
        let private PADDLE_LENGHT = 3.0f
        [<Literal>]
        let private PADDLE_HALF = 1.5f
        [<Literal>]
        let private PADDLE_WIDTH = 1.0f
       
        let public DEFAULT_GAME_STATE (p1Score : int, p2Score : int) =
            GameState(
                Ball(Vector(0.0f, 0.0f), Vector(BALL_SPEED_X * ((float32 (Random().Next(0, 1))) - 0.5f) * 2.0f, 0.0f)), //Ball
                PlayerData(Vector(-10.0f, 0.0f), p1Score), //Player 1
                PlayerData(Vector(10.0f, 0.0f), p2Score) //Player 2
                )
        

        ///Move player one down
        let moveDown (player: PlayerData) : PlayerData = PlayerData(Vector(
                                                                             player.Position.X, 
                                                                             Math.Min(Y_MAX - PADDLE_HALF, player.Position.Y + MOVE_DISTANCE_PLAYER)
                                                                          ), player.Score)
        
        ///Move player one up
        let moveUp (player: PlayerData) : PlayerData = PlayerData(Vector(
                                                                             player.Position.X, 
                                                                             Math.Min(Y_MIN + PADDLE_HALF, player.Position.Y - MOVE_DISTANCE_PLAYER)
                                                                          ), player.Score)
        ///Increment score of player
        let incrementScore (player: PlayerData) = PlayerData(player.Position, (player.Score + 1) )

        ///Restart game - reset positions of ball and players
        let restartGame (player1: PlayerData, player2: PlayerData) : GameState = 
            printfn "Ball has left the field : restarting game"
            printfn "Player1 score: %d " player1.Score
            printfn "Player2 score: %d " player2.Score
            DEFAULT_GAME_STATE(player1.Score, player2.Score)

        /// Check if ball will be hit or missed by a player the next time it moves. 
        /// If missed - increment score and restart positions
        let checkBounds (ball':Ball, player1:PlayerData, player2:PlayerData) : GameState =
            let rx = ball'.BallPosition.X
            let ry = ball'.BallPosition.Y
            let vy = ball'.BallDirection.Y//direction y

            if rx + ball'.BallDirection.X < (X_MIN + BALL_RADIUS) then //is ball at left edge
                if (((ry + vy) > player1.Position.Y + PADDLE_HALF ) ||             
                    ((ry + vy) < player1.Position.Y - PADDLE_HALF )) 
                then 
                    printfn "Player1 missed ball"
                    restartGame (player1, incrementScore player2)
                else 
                    printfn "Player1 hit ball"
                    GameState(ball',player1, player2)    

            else if rx + ball'.BallDirection.X > (X_MAX - BALL_RADIUS) then //is ball at right edge
                if (((ry + vy) > player2.Position.Y + PADDLE_HALF ) ||             //is ball hitting player1
                    ((ry + vy) < player2.Position.Y - PADDLE_HALF )) 
                then 
                    printfn "Player2 missed ball"
                    restartGame (incrementScore player1, player2)
                else 
                    printfn "Player2 hit ball"
                    GameState(ball', player1, player2)

            else
                GameState(ball', player1, player2)                            

        //Reflect ball; finds the angle for which the ball should be reflected upon hitting a paddle
        let reflectBall (ballPos:Vector, paddlePos:Vector) : Vector =
            let diffY = paddlePos.Y - ballPos.Y
            let normalizedDiffY = diffY / PADDLE_LENGHT
            let bounceAngle = float (normalizedDiffY * MAX_BOUNCE_ANGLE)
            let xDir = match ballPos.X < paddlePos.X with
                       | true  -> -1.0f
                       | false ->  1.0f
            Vector(xDir * BALL_SPEED_X * (float32 (Math.Cos bounceAngle)), BALL_SPEED_Y * (float32 (-Math.Sin bounceAngle)))

        //Is hitting paddle; helper for detecting simple intersection for Player1 / Player2.
        let isHittingPaddle (ball':Ball, playerPos:Vector, leftSide:bool) =
            if (leftSide && (ball'.BallPosition.X - BALL_RADIUS <= playerPos.X + PADDLE_WIDTH / 2.0f)) ||
               (not leftSide && (ball'.BallPosition.X + BALL_RADIUS >= playerPos.X - PADDLE_WIDTH / 2.0f)) then
                (ball'.BallPosition.Y < playerPos.Y + PADDLE_LENGHT / 2.0f) ||
                   (ball'.BallPosition.Y > playerPos.Y - PADDLE_LENGHT / 2.0f)
            else
                false

        ///Move ball; change direction if hitting top edge; bottom edge; player paddle
        let moveBall (ball':Ball, player1:PlayerData, player2:PlayerData) : Ball = 
            let rx = ball'.BallPosition.X
            let ry = ball'.BallPosition.Y
            
            // change direction when hitting top / bottom
            let v = if (ry + ball'.BallDirection.Y + BALL_RADIUS * 2.0f > Y_MAX) then Vector(ball'.BallDirection.X, -ball'.BallDirection.Y) // Hit bottom
                    elif (ry + ball'.BallDirection.Y + BALL_RADIUS * 2.0f < Y_MIN) then Vector(ball'.BallDirection.X, -ball'.BallDirection.Y) // Hit top
                    else if isHittingPaddle (ball', player1.Position, true) then reflectBall(ball'.BallPosition, player1.Position) // Hit p1 paddle
                    else if isHittingPaddle (ball', player2.Position, false) then reflectBall(ball'.BallPosition, player2.Position) // Hit p2 paddle
                    else ball'.BallDirection // Nothing hit
            
            Ball(Vector(rx + v.X, ry + v.Y), v)

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