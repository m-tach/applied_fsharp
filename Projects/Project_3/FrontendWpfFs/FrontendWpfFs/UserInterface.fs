module UserInterface

open System  
open FsXaml  
  
type App = XAML<"App.xaml">  

let run =
    App().Run()

