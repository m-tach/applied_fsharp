namespace Tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open SharedTypes.SharedTypes
open Client.StateMachine

[<TestClass>]
type ClientTests () =

    [<TestMethod>]
    member this.RunClientStateMachine () =
        Async.StartImmediate (start()) 
        ev.Post "Host game"
        Assert.IsTrue(false);
