namespace Server

open System.Net

open SharedTypes.NetworkStuff
open SharedTypes.SharedTypes
open SharedTypes.Constants
open Game
open SharedTypes.SharedTypes

module ComputerOpponent =

    type ComputerOpponentStateMachine () =
        let recs = NetworkReceiver(9003)
        let sends = NetworkSender(SERVER_PORT)
        let ev = AsyncEventQueue<SharedTypes.SharedTypes.Message>()
        
        do
            recs.StartListening()
            recs.ReceiveMessageEvent.Add(ev.Post)

        member private this.CalculateMove(playerId: int, gState: GameState) =
            match playerId with
            | 1 -> if (gState.Ball.BallPosition.Y < gState.Player1.Position.Y ) 
                   then Down
                   else Up
            | 2 -> if (gState.Ball.BallPosition.Y < gState.Player2.Position.Y ) 
                   then Down
                   else Up
            | _ -> failwith("Incorrect player id")               

        member public this.JoinGame() =
            async {
                printfn "state: computer JoinGame"
                do! sends.Send(JoinGame(IPAddress.Loopback), IPAddress.Loopback)
                return! this.YouJoinedTheGame();
            }         

        member public this.YouJoinedTheGame() =
            async {
                printfn "state: computer YouJoinedTheGame"; 

                let! msg = ev.Receive();
                printfn "parsing msg: computer %A" msg;
                match msg with
                | YouJoinedTheGame(playerId, ip) -> return! this.WaitForGameStart(playerId)
                | _ -> return! this.YouJoinedTheGame()
            }
        
        member private this.WaitForGameStart(playerId : int) =
            async {
                printfn "state: computer WaitForGameStart"; 

                let! msg = ev.Receive();
                printfn "parsing msg: computer %A" msg;
                match msg with
                | StartGame -> 
                return! this.SendInput(
                    playerId,                     
                    GameState(
                        Ball(Vector(0.0f, 0.0f), Vector(-0.2f, 0.2f)), //Ball
                        PlayerData(Vector(-10.0f, 0.0f), 0), //Player 1
                        PlayerData(Vector(10.0f, 0.0f), 0) //Player 2
                        ))
                | _ -> return! this.WaitForGameStart(playerId)
            }

        member private this.SendInput(playerId : int, gState: GameState) =
            async {
                printfn "state: computer SendInput";
                let computerMove = this.CalculateMove(playerId, gState)
                do! sends.Send(PlayerInput(playerId, computerMove), IPAddress.Loopback)
                return! this.WaitForGameState(playerId);
            }

        member private this.WaitForGameState(playerId : int) =
            async {
                printfn "state: computer WaitForGameStart"; 

                let! msg = ev.Receive();
                printfn "parsing msg: %A" msg;
                match msg with
                | GameStateUpdate gState -> return! this.SendInput(playerId, gState)
                | GameDone -> printfn("Computer opponent: Game over")
                | _ -> return! this.WaitForGameState(playerId);            

            }