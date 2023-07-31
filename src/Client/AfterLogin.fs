[<RequireQualifiedAccess>]
module AfterLogin

open Elmish
open Shared
open Feliz
open Feliz.Router
open Fable

type Model =
  { User: User; CurrentUrl : string list }

type Msg =
    | Logout
    | UrlChanged of string list

[<RequireQualifiedAccess>]
type Intent =
    | LogoutUser of User
    | DoNothing

let init(user: User) =
    { User = user; CurrentUrl = Router.currentUrl() }, Cmd.none

let update (msg: Msg) (model: Model) =
    match msg with
    | UrlChanged url -> { model with CurrentUrl = url }, Cmd.none, Intent.DoNothing
    | Logout -> model, Cmd.none, Intent.LogoutUser model.User

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

let getHomePageContent (model: Model) (dispatch: Msg -> unit) =
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

let render (model: Model) (dispatch: Msg -> unit) =
    Html.div [
        getHomePageContent model dispatch
        let activePage =
            match model.CurrentUrl with
            | [ ] -> Html.none
            | [ "about" ] -> Html.h1 "About"
            | [ "contact" ] -> Html.h1 "Contact"
            | _ -> Html.h1 "Not Found"

        React.router [
            router.onUrlChanged (UrlChanged >> dispatch)
            router.children [ activePage ]
        ]
    ]
    