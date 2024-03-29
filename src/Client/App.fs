module App

open Elmish
open Elmish.React

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

//Program.mkProgram Index.init Index.update Index.view
Program.mkProgram LoginIndex.init LoginIndex.update LoginIndex.render
//Program.mkProgram UrlListener.init UrlListener.update UrlListener.render
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactSynchronous "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run