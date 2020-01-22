namespace SharedTypes

open System
open System.Net
open System.Net.Sockets
open System.Net.NetworkInformation;
open System.Threading

module NetworkStuff =
    let private convertToBroadcast (uni:IPAddress) (sub:IPAddress) =
        let broadcastBytes = Array.map2(fun x y -> x ||| ~~~y) (uni.GetAddressBytes()) (sub.GetAddressBytes())
        IPAddress(broadcastBytes)

    let private getBroadcastAddresses: IPAddress array =
        Array.collect(fun (x: NetworkInterface) -> 
            if not x.IsReceiveOnly && x.NetworkInterfaceType <> NetworkInterfaceType.Loopback && x.OperationalStatus = OperationalStatus.Up
            then // In order to unpack the unique collection from x.GetIPProperties().UnicastAddresses
                 // this was necessary
                 let bList = Collections.Generic.List<IPAddress>()
                 for i in x.GetIPProperties().UnicastAddresses do
                    if i.IsDnsEligible
                    then bList.Add(convertToBroadcast (i.Address) (i.IPv4Mask))
                 bList.ToArray()              
            else Array.empty) (NetworkInterface.GetAllNetworkInterfaces())

    let public Broadcast(bytes, port) =
        async {               
            Array.iter(fun (x:IPAddress) ->
                use broadcastClient = new UdpClient()
                broadcastClient.ExclusiveAddressUse <- false
                broadcastClient.Client.SetSocketOption(SocketOptionLevel.Socket, SocketOptionName.ReuseAddress, true) 
                broadcastClient.EnableBroadcast <- true

                broadcastClient.Connect(IPEndPoint(x, port))
                broadcastClient.Send(bytes, bytes.Length) |> ignore
                ) getBroadcastAddresses
        }    


    type public NetworkSender(port: int, recAddr: IPAddress) =
        let port = port
        let receiverAddress = recAddr
        let client = new UdpClient()
        do
            client.ExclusiveAddressUse <- false
            client.Client.SetSocketOption(SocketOptionLevel.Socket, SocketOptionName.ReuseAddress, true)
            client.Connect(IPEndPoint(receiverAddress, port))

        member public this.Send(bytes) =
            async { client.SendAsync(bytes, bytes.Length) |> ignore }

    type public NetworkReceiver(port: int) =
        let port = port
        let listener = new UdpClient()
        let receiveEvent = new Event<byte array>()
        do
            listener.ExclusiveAddressUse <- false
            listener.Client.SetSocketOption(SocketOptionLevel.Socket, SocketOptionName.ReuseAddress, true)
            listener.Client.Bind(IPEndPoint(IPAddress.Any, port))

        [<CLIEvent>]
        member public this.ReceiveMessageEvent = receiveEvent.Publish 

        member public this.StartListening() =
            listener.BeginReceive(AsyncCallback(this.Receive), null) |> ignore

        member private this.Receive (result: IAsyncResult) =

            //get message
            let endPoint = IPEndPoint(IPAddress.Any, port)
            let message = listener.EndReceive(result, ref endPoint)

            //start listening again
            listener.BeginReceive(AsyncCallback(this.Receive), null) |> ignore

            //make a receive event.
            //This method may be executed at the same time from multiple threads
            //so it's necessary to synchronize the execution of the events
            //as that's what the rest of the code would expect
            lock listener (fun () -> receiveEvent.Trigger(message))


