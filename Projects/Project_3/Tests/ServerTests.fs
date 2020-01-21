namespace Tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open SharedTypes.SharedTypes
open Server.StateMachine
open Server.GameEngine

[<TestClass>]
type ServerTests () =

    [<TestMethod>]
    member this.RunServerStateMachine () =
        ev.Post "Start"
        Assert.IsTrue(true);
