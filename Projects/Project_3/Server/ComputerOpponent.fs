namespace Server

open System.Net

open SharedTypes.NetworkStuff
open SharedTypes.SharedTypes
open SharedTypes.Constants
open Game

module ComputerOpponent =

    type ComputerOpponentStateMachine (sender : NetworkSender, ev : AsyncEventQueue<SharedTypes.SharedTypes.Message>) =
        
        member public this.Listen() =
            async {
                let! msg = ev.Receive()
                match msg with
                | YouJoinedTheGame(playerId, ip) -> this.ListenGame(playerId)
            }
        
        member private this.ListenGame(playerId : int) =
            async {
                let! msg = ev.Receive()
                match msg with
                | _ -> printfn "got msg!"
            }