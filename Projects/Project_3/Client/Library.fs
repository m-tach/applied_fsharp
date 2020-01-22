namespace Client


/// Show "Host game | Join game"
/// "Host game" -> launch Server
/// "Join game" -> get available servers; connect to 1;
/// "Start game" -> 
///     send input periodically (if nothing captured -> empty input); 
///     receive game state
/// "Leave game"  
module Say =
    let hello name =
        printfn "Hello %s" name
