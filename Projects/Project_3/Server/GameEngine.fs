namespace Server
    open System
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
            printfn "Player1 score: %d " (snd player1)
            printfn "Player2 score: %d " (snd player2)
            (
                ((0.0, 0.0), (-1.0, 1.0)), //Ball
                ((-10.0, 0.0), snd player1), //Player 1
                ((10.0, 0.0), snd player2) //Player 2
            ) 

        let checkBounds (ball':Ball, player1:PlayerData, player2:PlayerData) : GameState =
            let rx = fst (fst ball')
            let ry = snd (fst ball')
            let vy = snd (snd ball')

            if rx +  fst (snd ball') < -9.0 then //is ball at left edge
                if (((ry + vy) > (fst (fst player1) + 1.5)) ||             
                    ((ry + vy) < fst (fst player1) - 1.5)) 
                then 
                    printfn "Player1 missed ball"
                    restartGame (player1, incrementScore player2)
                else 
                    printfn "Player1 hit ball"
                    (ball',player1, player2)    

            else if rx +  fst (snd ball') > 9.0 then //is ball at right edge
                if (((ry + vy) > (fst (fst player2) + 1.5)) ||             //is ball hitting player1
                    ((ry + vy) < fst (fst player2) - 1.5)) 
                then 
                    printfn "Player2 missed ball"
                    restartGame (incrementScore player1, player2)
                else 
                    printfn "Player2 hit ball"
                    (ball', player1, player2)

            else
                (ball', player1, player2)                            

        ///Move ball; change direction if hitting top edge; bottom edge; player paddle
        ///Change score if gone through edge and restart from center
        let moveBall (ball':Ball, player1:PlayerData, player2:PlayerData) : Ball = 
            let rx = fst (fst ball')
            let ry = snd (fst ball')
            
            // change direction when hitting top / bottom
            let vy = if (ry + snd (snd ball') + ballRadius > 10.0) then -(snd (snd ball'))
                     elif (ry + snd (snd ball') + ballRadius < -10.0) then -(snd (snd ball'))
                     else snd (snd ball')
                                 
            let vx = if rx +  fst (snd ball') < -9.0 then //is ball at left edge
                        if (((ry + vy) < (fst (fst player1) + 1.5)) ||             //is ball hitting player1
                            ((ry + vy) > fst (fst player1) - 1.5)) 
                            then -(fst (snd ball'))
                        else 
                            1.0                                                
                     elif rx +  fst (snd ball') > 9.0 then //is ball at right edge
                        if (((ry + vy) < fst (fst player2) + 1.5) ||             //is ball hitting player2
                            ((ry + vy) > fst (fst player2) - 1.5)) 
                            then -(fst (snd ball'))
                        else
                            1.0                                                
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
            let (outside, p1', p2') = checkBounds(ball, p1, p2)
            let newBall = moveBall (outside, p1', p2')

            (newBall, p1', p2')