namespace Tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open SharedTypes.SharedTypes
open Server.ServerStateMachine
open Server.GameEngine

[<TestClass>]
type ServerTests () =

    [<TestMethod>]
    member this.RunServerStateMachine () =
        ev.Post StartGame
        Assert.IsTrue(true);
