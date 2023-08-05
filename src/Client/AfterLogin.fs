[<RequireQualifiedAccess>]
module AfterLogin

open Elmish
open Shared
open Feliz
open Feliz.Router
open Fable
open System
open UrlType

[<RequireQualifiedAccess>]
type Page =
  | Home
  | Index of Index.Model
  | NotFound

type Model =
  { User: User; CurrentPage: Page; CurrentUrl : Url }

type Msg =
    | Logout
    | UrlChanged of Url
    | IndexMsg of Index.Msg
    | ChangePageToIndex

[<RequireQualifiedAccess>]
type Intent =
    | LogoutUser of User
    | DoNothing

let init(user: User) =
    { User = user; CurrentPage = Page.Home; CurrentUrl = parseUrl(Router.currentUrl())}, Cmd.none

let update (msg: Msg) (model: Model) =
    match msg, model.CurrentPage with
    | UrlChanged url, _ ->
        Console.WriteLine($"outer url: {url}")
        let getModelWithPageAndCmd =
            match url with
            | Url.BooklistUrl _ ->
                let indexModel, indexCmd = Index.init()
                { model with CurrentPage = Page.Index indexModel; },Cmd.map IndexMsg indexCmd
            | Url.AfterLogin ->
                let homeModel, homeCmd = init (model.User)
                homeModel, homeCmd
            | Url.NotFound -> { model with CurrentPage = Page.NotFound }, Cmd.none
        let updatedPage, cmd = getModelWithPageAndCmd
        { updatedPage with CurrentUrl = url}, cmd, Intent.DoNothing
    | Logout, _ -> model, Cmd.none, Intent.LogoutUser model.User
    | IndexMsg indexMsg, Page.Index indexModel ->
        let updatedIndexModel, indexCmd = Index.update indexMsg indexModel
        { model with CurrentPage = Page.Index updatedIndexModel }, Cmd.map IndexMsg indexCmd, Intent.DoNothing
    | _, _ -> model, Cmd.none, Intent.DoNothing

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
    Html.div [
        centered [
            Html.div [
                Html.h1 [
                    Html.strong (model.User.Username.ToUpper())
                ]

                Html.button [
                    prop.className "button is-info"
                    prop.onClick (fun _ -> dispatch Logout)
                    prop.text "Logout"
                ]
            ]
        ]

        Html.a [
                prop.text "Booklist"
                prop.href (Router.format(["booklist"; "books"]))
                prop.style [ style.margin 5 ]
        ]
    ]
    

let render (model: Model) (dispatch: Msg -> unit) =
    Html.div [
        getHomePageContent model dispatch       
        React.router [
            router.onUrlChanged (parseUrl >> UrlChanged >> dispatch)
        ]
        match model.CurrentPage with
        | Page.Home -> Html.none
        | Page.Index model -> Index.render model (Msg.IndexMsg >> dispatch)
        | Page.NotFound ->
            centered [
                Html.h1 [
                    Html.strong "Page not found"
                ]
            ]
    ]