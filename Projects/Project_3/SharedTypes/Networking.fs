namespace SharedTypes

open System.Net
open System.Net.Sockets
open System.Net.NetworkInformation;
open System

module NetworkStuff =
    let convertToBroadcast (uni:IPAddress) (sub:IPAddress) =
        let broadcastBytes = Array.map2(fun x y -> x ||| ~~~y) (uni.GetAddressBytes()) (sub.GetAddressBytes())
        IPAddress(broadcastBytes)

    let getBroadcastAddresses: IPAddress array =
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

    type NetworkSender(port: int, recAddr: IPAddress) =
        let port = port
        let receiverAddress = recAddr
        let client = new UdpClient(port)
        let mutable broadcastAddresses = Array.empty
        let getBroadcastAddresses =
            if broadcastAddresses.Length = 0
            then broadcastAddresses <- getBroadcastAddresses
            broadcastAddresses

        member this.Broadcast(bytes) =
            Array.map(fun (x:IPAddress) ->
                client.Client.Bind(IPEndPoint(x, port))
                client.SendAsync(bytes, bytes.Length)
                ) getBroadcastAddresses

        member this.Send(bytes) =
            client.Client.Bind(IPEndPoint(receiverAddress, port))
            client.SendAsync(bytes, bytes.Length)

    type NetworkReceiver(port: int) =
        let port = port
        let listener = new UdpClient()
        let receiveEvent = new Event<_>()

        member this.StartListening() =
            

        [<CLIEvent>]
        member this.ReceiveMessageEvent = receiveEvent.Publish

        member this.TestEvent(arg) =
            event1.Trigger(this, arg)            


