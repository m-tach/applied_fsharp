namespace SharedTypes

open System
open System.IO
open System.Net

module SharedTypes =
    type Vector(x: float32, y: float32) = 
        member public this.X = x
        member public this.Y = y

        member public this.ToBytes() =
            use storageStream = new MemoryStream()
            use byteWriter = new BinaryWriter(storageStream)
            this.ToStream(byteWriter)
            storageStream.ToArray()

        member public this.ToStream(byteWriter: BinaryWriter) = 
            byteWriter.Write(this.X)
            byteWriter.Write(this.Y)

        static member public FromBytes(bytes: byte array) =
            use storageStream = new MemoryStream(bytes)
            use byteReader = new BinaryReader(storageStream)
            Vector.FromStream(byteReader)

        static member public FromStream(stream: BinaryReader) =
            Vector(stream.ReadSingle(), stream.ReadSingle())            

    //              player pos  score
    type PlayerData(playerPos: Vector, score: int) =
        member public this.Position = playerPos
        member public this.Score = score        

        member public this.ToBytes() =
            use storageStream = new MemoryStream()
            use byteWriter = new BinaryWriter(storageStream)
            this.ToStream(byteWriter)
            storageStream.ToArray()

        member public this.ToStream(byteWriter: BinaryWriter) = 
            this.Position.ToStream(byteWriter)
            byteWriter.Write(this.Score)

        static member public FromBytes(bytes: byte array) =
            use storageStream = new MemoryStream(bytes)
            use byteReader = new BinaryReader(storageStream)
            PlayerData.FromStream(byteReader)

        static member public FromStream(stream: BinaryReader) =
            PlayerData(Vector.FromStream(stream), stream.ReadInt32())

    //         ball pos  ball dir
    type Ball(ballPos: Vector, ballDir: Vector) =
        member public this.BallPosition = ballPos
        member public this.BallDirection = ballDir

        member public this.ToBytes() =
            use storageStream = new MemoryStream()
            use byteWriter = new BinaryWriter(storageStream)
            this.ToStream(byteWriter)
            storageStream.ToArray()

        member public this.ToStream(byteWriter: BinaryWriter) = 
            this.BallPosition.ToStream(byteWriter)
            this.BallDirection.ToStream(byteWriter)

        static member public FromBytes(bytes: byte array) =
            use storageStream = new MemoryStream(bytes)
            use byteReader = new BinaryReader(storageStream)
            Ball.FromStream(byteReader)

        static member public FromStream(stream: BinaryReader) =
            Ball(Vector.FromStream(stream), Vector.FromStream(stream))

    type GameState(ball: Ball, player1: PlayerData, player2: PlayerData) = 
        member public this.Ball = ball
        member public this.Player1 = player1
        member public this.Player2 = player2

        member public this.ToBytes() =
            use storageStream = new MemoryStream()
            use byteWriter = new BinaryWriter(storageStream)
            this.ToStream(byteWriter)
            storageStream.ToArray()

        member public this.ToStream(byteWriter: BinaryWriter) = 
            this.Ball.ToStream(byteWriter)
            this.Player1.ToStream(byteWriter)
            this.Player2.ToStream(byteWriter)

        static member public FromBytes(bytes: byte array) =
            use storageStream = new MemoryStream(bytes)
            use byteReader = new BinaryReader(storageStream)
            GameState.FromStream(byteReader)

        static member public FromStream(stream: BinaryReader) =
            GameState(Ball.FromStream(stream), PlayerData.FromStream(stream), PlayerData.FromStream(stream))

    type Input =
        | Up 
        | Down 
        | Escape

        member public this.ToBytes() =
            match this with
            | Up     -> [|0uy|]
            | Down   -> [|1uy|]
            | Escape -> [|2uy|]

        member public this.ToStream(byteWriter: BinaryWriter) = 
            match this with
            | Up     -> byteWriter.Write(0uy)
            | Down   -> byteWriter.Write(1uy)
            | Escape -> byteWriter.Write(2uy)

        static member public FromBytes(bytes: byte array) =
            match bytes with
            | [|0uy|] -> Up
            | [|1uy|] -> Down
            | [|2uy|] -> Escape
            | _ -> failwith (String.Format("failed to convert byte array to input.\nByte array content: {0}", String.Join(", ", bytes)))

        static member public FromStream(stream: BinaryReader) =
            match stream.ReadByte() with
            | 0uy -> Up
            | 1uy -> Down
            | 2uy -> Escape
            | a -> failwith (String.Format("failed to convert byte to input.\nByte: {0}", a))

    type GameServer(serverName: string, address: IPAddress) = 
        member public this.ServerName = serverName
        member public this.Address = address

        member public this.ToBytes() =
            use storageStream = new MemoryStream()
            use byteWriter = new BinaryWriter(storageStream)
            this.ToStream(byteWriter)
            storageStream.ToArray()

        member public this.ToStream(byteWriter: BinaryWriter) = 
            let stringAsBytes = System.Text.Encoding.UTF8.GetBytes(this.ServerName)
            byteWriter.Write(stringAsBytes, 0, stringAsBytes.Length)
            let addressAsBytes = System.Text.Encoding.UTF8.GetBytes(this.Address.ToString())
            byteWriter.Write(addressAsBytes, 0, addressAsBytes.Length)

        static member public FromBytes(bytes: byte array) =
            use storageStream = new MemoryStream(bytes)
            use byteReader = new BinaryReader(storageStream)
            GameServer.FromStream(byteReader)

        static member public FromStream(stream: BinaryReader) =
            GameServer(stream.ReadString(), IPAddress.Parse(stream.ReadString()))

    type public Message = 
       | RequestServers of IPAddress
       | Server of GameServer
       | JoinGame of IPAddress
       | YouJoinedTheGame of int * IPAddress
       | StartGame
       | GameDone
       | GameStateUpdate of GameState
       | PlayerInput of int * Input
       | HostGame of string

        member public this.ToBytes() =
            use storageStream = new MemoryStream()
            use byteWriter = new BinaryWriter(storageStream)
            match this with
            | RequestServers(ip) -> byteWriter.Write(0uy)
                                    let addressAsBytes = System.Text.Encoding.UTF8.GetBytes(ip.ToString())
                                    byteWriter.Write(addressAsBytes, 0, addressAsBytes.Length)
            | Server(gs) -> byteWriter.Write(1uy)
                            gs.ToStream(byteWriter)
            | JoinGame(ip) -> byteWriter.Write(2uy)
                              let addressAsBytes = System.Text.Encoding.UTF8.GetBytes(ip.ToString())
                              byteWriter.Write(addressAsBytes, 0, addressAsBytes.Length)
            | YouJoinedTheGame(p, ip) -> byteWriter.Write(3uy)
                                         byteWriter.Write(p)
                                         let addressAsBytes = System.Text.Encoding.UTF8.GetBytes(ip.ToString())
                                         byteWriter.Write(addressAsBytes, 0, addressAsBytes.Length)                                     
            | StartGame -> byteWriter.Write(4uy)
            | GameDone -> byteWriter.Write(5uy)
            | GameStateUpdate(s) -> byteWriter.Write(6uy)
                                    s.ToStream(byteWriter)
            | PlayerInput(p, i) -> byteWriter.Write(7uy)
                                   byteWriter.Write(p)
                                   i.ToStream(byteWriter)
            storageStream.ToArray()                     
        static member public FromBytes(bytes: byte array) =
            use storageStream = new MemoryStream(bytes)
            use byteReader = new BinaryReader(storageStream)
            match byteReader.ReadByte() with
            | 0uy -> RequestServers(IPAddress.Parse(byteReader.ReadString()))
            | 1uy -> Server(GameServer.FromStream(byteReader))
            | 2uy -> JoinGame(IPAddress.Parse(byteReader.ReadString()))
            | 3uy -> YouJoinedTheGame(byteReader.ReadInt32(), IPAddress.Parse(byteReader.ReadString()))
            | 4uy -> StartGame
            | 5uy -> GameDone
            | 6uy -> GameStateUpdate(GameState.FromStream(byteReader))
            | 7uy -> PlayerInput(byteReader.ReadInt32(), Input.FromStream(byteReader))
            | a -> failwith (String.Format("failed to convert byte to Message.\nByte: {0}", a))




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
