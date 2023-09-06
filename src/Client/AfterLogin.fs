[<RequireQualifiedAccess>]
module AfterLogin

open Elmish
open Shared
open Feliz
open Feliz.Router
open Fable
open ViewUtils
open System
open Feliz.Bulma
open UrlLookup

[<RequireQualifiedAccess>]
type Page =
  | Home
  | Index of Index.Model
  | NotFound

[<RequireQualifiedAccess>]
type Url =
  | AfterLogin
  | Booklist of Index.Url
  | NotFound

let parseUrl = function
    | ["home"] | [] -> Url.AfterLogin
    | "booklist" :: booklistSegment -> Url.Booklist (Index.parseUrl booklistSegment)
    | _ -> Url.NotFound

type Model =
  { User: User; CurrentPage: Page; CurrentUrl : Url }

type Msg =
    | Logout
    | UrlChanged of Url
    | IndexMsg of Index.Msg

[<RequireQualifiedAccess>]
type Intent =
    | LogoutUser of User
    | DoNothing

let init(user: User, url: Url) =
    let initialModel = { User = user; CurrentPage = Page.NotFound; CurrentUrl = url}
    match url with
    | Url.AfterLogin ->
        { initialModel with CurrentPage = Page.Home }, Cmd.none
    | Url.Booklist booklistUrl ->
        let indexModel, indexCmd = Index.init booklistUrl
        { initialModel with CurrentPage = Page.Index indexModel; }, Cmd.map IndexMsg indexCmd
    | Url.NotFound ->
        initialModel, Cmd.none

let handleUrlChange url model =
    let getModelWithPageAndCmd =
        match url with
        | Url.Booklist booklistUrl ->
            let indexModel, indexCmd = Index.init booklistUrl
            { model with CurrentPage = Page.Index indexModel; }, Cmd.map IndexMsg indexCmd
        | Url.AfterLogin ->
            let homeModel, homeCmd = init (model.User, Url.AfterLogin)
            homeModel, homeCmd
        | Url.NotFound -> { model with CurrentPage = Page.NotFound }, Cmd.none
    let updatedPage, cmd = getModelWithPageAndCmd
    { updatedPage with CurrentUrl = url}, cmd, Intent.DoNothing

let update (msg: Msg) (model: Model) =
    match msg, model.CurrentPage with
    | UrlChanged url, _ ->
        handleUrlChange url model
    | Logout, _ ->
        model, Cmd.none, Intent.LogoutUser model.User
    | IndexMsg indexMsg, Page.Index indexModel ->
        let updatedIndexModel, indexCmd = Index.update indexMsg indexModel
        { model with CurrentPage = Page.Index updatedIndexModel }, Cmd.map IndexMsg indexCmd, Intent.DoNothing
    | _, _ ->
        model, Cmd.none, Intent.DoNothing

let getHomePageContent (model: Model) (dispatch: Msg -> unit) =
    let getNavbarStart () =
        Bulma.navbarStart.div [
            Bulma.navbarItem.a [ prop.text "Books"; prop.href (Router.format(UrlLookup.getUrlForPage UrlLookup.Page.Booklist)) ]
            Bulma.navbarItem.a [ prop.text "Authors"; prop.href (Router.format(UrlLookup.getUrlForPage UrlLookup.Page.Authorslist)) ]
            Bulma.navbarItem.a [ prop.text "Add book"; prop.href (Router.format(UrlLookup.getUrlForPage UrlLookup.Page.CreateBook)) ]
            Bulma.navbarItem.a [ prop.text "Add author"; prop.href (Router.format(UrlLookup.getUrlForPage UrlLookup.Page.CreateAuthor)) ]
            Bulma.navbarItem.a [ prop.text "Contact" ]
            Bulma.navbarItem.a [ prop.text "About" ]
        ]

    let getNavbarEnd () =
        Bulma.navbarEnd.div [
            Bulma.navbarItem.div [
                Bulma.media [
                    Bulma.mediaContent [
                        Bulma.text.hasTextRight
                        prop.children [
                            Bulma.title.p [
                                Bulma.text.hasTextWeightBold
                                Bulma.title.is6
                                prop.text model.User.Username
                            ]
                            Bulma.subtitle.p [
                                Bulma.title.is6
                                Bulma.text.hasTextWeightLight
                                Bulma.color.hasTextGreyLighter
                                prop.text $"@{model.User.Username.ToLower()}"
                            ]
                        ]
                    ]
                ]
            ]
            Bulma.navbarItem.div [
                Bulma.buttons [
                    Bulma.button.a [
                        prop.className "button is-info"
                        prop.onClick (fun _ -> dispatch Logout)
                        prop.children [
                            Html.strong "Logout"
                        ]
                    ]
                ]
            ]
        ]

    let getPageContent () =
        match model.CurrentPage with
        | Page.Home -> Html.none
        | Page.Index model -> Index.render model (Msg.IndexMsg >> dispatch)
        | Page.NotFound ->
            centered [
                Html.h1 [
                    Html.strong "Page not found"
                ]
            ]

    Bulma.hero [
        hero.isFullHeight
        color.isPrimary
        prop.style [
            style.backgroundSize "cover"
            style.backgroundImageUrl "/bookshelf.jpg"
            style.backgroundPosition "no-repeat center center fixed"
        ]
        prop.children [
            Bulma.heroHead [
                Bulma.navbar [
                    prop.children [
                        Bulma.navbarBrand.div [
                            Bulma.navbarItem.a [
                                prop.href (Router.format(["home"]))
                                prop.children [
                                    Html.img [ prop.src "https://bulma.io/images/bulma-logo-white.png"; prop.height 28; prop.width 112; ]
                                ]
                            ]
                        ]
                        Bulma.navbarMenu [
                            getNavbarStart ()
                            getNavbarEnd ()
                        ]
                    ]
                ]
            ]
            Bulma.heroBody [
                Bulma.container [
                    Bulma.column [
                        column.is6
                        column.isOffset3
                        prop.children [
                            Bulma.title [
                                text.hasTextCentered
                                prop.text "FsSafeApplication"
                            ]
                            getPageContent ()
                        ]
                    ]
                    Html.div [
                        prop.style [
                            style.margin.auto
                            style.textAlign.center
                            style.width (length.percent 100)
                        ]
                    ]
                ]
            ]
        ]
    ]
    

let render (model: Model) (dispatch: Msg -> unit) =
    Html.div [
        getHomePageContent model dispatch       
        React.router [
            router.onUrlChanged (parseUrl >> UrlChanged >> dispatch)
        ]
    ]