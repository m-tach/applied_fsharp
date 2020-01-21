namespace SharedTypes

module SharedTypes =
    type Vector = float * float
    //              player pos  score
    type PlayerData = Vector * int
    //         ball pos  ball dir
    type Ball = Vector * Vector
    type GameState = Ball * PlayerData * PlayerData 
    type Input = Up | Down | Escape
