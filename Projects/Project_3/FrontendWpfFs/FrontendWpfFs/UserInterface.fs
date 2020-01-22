module UserInterface

open System  
open FsXaml  
open System.Windows.Controls
  
type App = XAML<"App.xaml">

type GameServer (name : string, ip : string) =
    member val Name = name
    member val IPAddress = ip

let app = App()

let run =
    printfn "About to run..."
    app.Run() |> ignore
    printfn "Has ran."

let private findName (name : string) = app.MainWindow.FindName(name)

let displayGameLobby (games : GameServer list) =
    let list = downcast (findName "GameServerList") : ListView
    list.Items.Clear()

    List.iter (fun game -> list.Items.Add game |> ignore) games

app.Startup.Add(fun _ ->  app.MainWindow.ContentRendered.Add(fun _ ->   printf "Startup done!"
                                                                        displayGameLobby [
                                                                            GameServer ("Hello world", "ABCDEFGH");
                                                                            GameServer ("World Hello", "BCDEFGHI");
                                                                            GameServer ("Hello Hello", "CDEFGHIJ")
                                                                        ])
                                )