module UrlListener

open Elmish
open Feliz
open Feliz.Router

type Model = { CurrentUrl : string list }

type Msg =
    | UrlChanged of string list

let init() =
    { CurrentUrl = Router.currentUrl() }, Cmd.none

let update (msg: Msg) (model: Model) =
    match msg with
    | UrlChanged url -> { model with CurrentUrl = url }, Cmd.none

let render state dispatch =
    let activePage =
        match state.CurrentUrl with
        | [ ] -> Html.h1 "Home"
        | [ "about" ] -> Html.h1 "About"
        | [ "contact" ] -> Html.h1 "Contact"
        | _ -> Html.h1 "Not Found"

    React.router [
        router.onUrlChanged (UrlChanged >> dispatch)
        router.children [ activePage ]
    ]

