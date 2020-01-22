namespace Client

open SharedTypes.SharedTypes

module ClientStuff =

    type Client() =
        let newGameServerFound = new Event<GameServer>()
        let newGameState = new Event<GameState>()

        [<CLIEvent>]
        member public this.NewGameServerFoundEvent = newGameServerFound.Publish
        [<CLIEvent>]
        member public this.NewGameStateEvent = newGameState.Publish


        member public this.JoinGame(server: GameServer) : bool =
            true

        member public this.KeyPressed(key: char) =
            "something" |> ignore                  
