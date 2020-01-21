// Learn more about F# at http://fsharp.org

open System

[<EntryPoint>]
let main argv =
    let receiver = NetworkReceiver(9001)
    let sender = NetworkSender(9001, IPAddress.Loopback)

    receiver.ReceiveMessageEvent.Add(fun x -> printfn "%s" (System.Text.Encoding.UTF8.GetString(x)))

    receiver.StartListening()

    Async.RunSynchronously (Broadcast(System.Text.Encoding.UTF8.GetBytes("the thing!"), 9001))
    Async.RunSynchronously (Broadcast(System.Text.Encoding.UTF8.GetBytes("the thing!"), 9001))
    Async.RunSynchronously (Broadcast(System.Text.Encoding.UTF8.GetBytes("the thing!"), 9001))
    Async.RunSynchronously (Broadcast(System.Text.Encoding.UTF8.GetBytes("the thing!"), 9001))


    Async.RunSynchronously (sender.Send(System.Text.Encoding.UTF8.GetBytes("the taahing!"))) |> ignore
    Async.RunSynchronously (sender.Send(System.Text.Encoding.UTF8.GetBytes("the taahing!"))) |> ignore
    Async.RunSynchronously (sender.Send(System.Text.Encoding.UTF8.GetBytes("the taahing!"))) |> ignore
    Async.RunSynchronously (sender.Send(System.Text.Encoding.UTF8.GetBytes("the taahing!"))) |> ignore

    Async.RunSynchronously (Async.Sleep(3000))

    0 // return an integer exit code
