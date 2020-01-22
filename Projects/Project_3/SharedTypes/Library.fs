namespace SharedTypes

module SharedTypes =
    type Vector = float * float
    //              player pos  score
    type PlayerData = Vector * int
    //         ball pos  ball dir
    type Ball = Vector * Vector
    type GameState = Ball * PlayerData * PlayerData 
    type Input = Up | Down | Escape

    type GameServer = int

    // An asynchronous event queue kindly provided by Don Syme 
    type AsyncEventQueue<'T>() = 
        let mutable cont = None 
        let queue = System.Collections.Generic.Queue<'T>()
        let tryTrigger() = 
            match queue.Count, cont with 
            | _, None -> ()
            | 0, _ -> ()
            | _, Some d -> 
                cont <- None
                d (queue.Dequeue())

        let tryListen(d) = 
            if cont.IsSome then invalidOp "multicast not allowed"
            cont <- Some d
            tryTrigger()

        member x.Post msg = queue.Enqueue msg; tryTrigger()
        member x.Receive() = 
            Async.FromContinuations (fun (cont,econt,ccont) -> 
                tryListen cont)
