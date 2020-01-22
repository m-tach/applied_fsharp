namespace Tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open SharedTypes.SharedTypes
open Client.ClientStateMachine

[<TestClass>]
type ClientTests () =

    [<TestMethod>]
    member this.RunClientStateMachine () =
        Async.StartImmediate (startLobby()) 
        ev.Post HostGame
        Assert.IsTrue(false);
