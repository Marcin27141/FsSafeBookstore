[<RequireQualifiedAccess>]
module AfterLogin

open Elmish
open Shared
open Fable.React
open Feliz

type Model =
  { User: User }

type Msg =
    | Logout

let init(user: User) =
    { User = user }, Cmd.none

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | Logout -> model, Cmd.none

let centered (children: ReactElement list) =
    Html.div [
        prop.style [
            style.margin.auto
            style.textAlign.center
            style.padding 20
            style.width (length.percent 100)
        ]

        prop.children children
    ]

let render (model: Model) (dispatch: Msg -> unit) =
    centered [
        Html.h1 [
            Html.strong (model.User.Username.ToUpper())
        ]

        Html.button [
            prop.className "button is-info"
            prop.onClick (fun _ -> dispatch Logout)
            prop.text "Logout"
        ]
    ]