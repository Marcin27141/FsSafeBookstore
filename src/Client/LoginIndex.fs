[<RequireQualifiedAccess>]
module LoginIndex

open Feliz.Router
open Elmish
open UrlLookup
open Shared
open Feliz
open Fable
open Feliz.Bulma

[<RequireQualifiedAccess>]
type Page =
  | Welcome
  | Login of Login.Model
  | Home of AfterLogin.Model
  | AccessDenied

[<RequireQualifiedAccess>]
type Url =
  | Welcome
  | Login
  | Home of AfterLogin.Url

type Model =
  { CurrentPage: Page; CurrentUrl : Url; User: Option<User> }

let parseUrl = function
    | [] -> Url.Welcome
    | ["login"] -> Url.Login
    | url -> Url.Home (AfterLogin.parseUrl url)

type Msg =
    | LoginMsg of Login.Msg
    | HomeMsg of AfterLogin.Msg
    | UrlChanged of Url

let init() =
    { CurrentPage = Page.Welcome; CurrentUrl = Url.Welcome; User = None }, Cmd.none

let handleUrlChange url model =
    let getModelWithPageAndCmd =
        match url with
        | Url.Welcome when model.User.IsNone ->
            { model with CurrentPage = Page.Welcome }, Cmd.none
        | Url.Welcome when model.User.IsSome ->
            let homeModel, homeCmd = AfterLogin.init (model.User.Value, AfterLogin.parseUrl([]))
            { model with CurrentPage = Page.Home homeModel}, Cmd.map HomeMsg homeCmd
        | Url.Home homeUrl when model.User.IsSome ->
            let homeModel, homeCmd = AfterLogin.init (model.User.Value, homeUrl)
            { model with CurrentPage = Page.Home homeModel}, Cmd.map HomeMsg homeCmd
        | Url.Login ->
            let loginModel, loginCmd = Login.init()
            { model with CurrentPage = Page.Login loginModel}, Cmd.map LoginMsg loginCmd
        | _ ->
            { model with CurrentPage = Page.AccessDenied }, Cmd.none
    let updatedPage, cmd = getModelWithPageAndCmd
    { updatedPage with CurrentUrl = url}, cmd

let updateLoginModel loginMsg loginModel model =
    let updatedLoginModel, loginCmd, intent = Login.update loginMsg loginModel
    match intent with
    | Login.Intent.UserLoggedIn user ->
        { model with User = Some user }, Cmd.navigate(getUrlForPage UrlLookup.Page.Home)
    | Login.Intent.DoNothing ->
        { model with CurrentPage = Page.Login updatedLoginModel }, Cmd.map LoginMsg loginCmd

let updateHomeModel homeMsg homeModel model =
    let updatedModel, cmd, intent = AfterLogin.update homeMsg homeModel
    match intent with
    | AfterLogin.Intent.LogoutUser user ->       
        { model with User = None}, Cmd.navigate(getUrlForPage UrlLookup.Page.Login) 
    | AfterLogin.Intent.DoNothing ->
        { model with CurrentPage = Page.Home updatedModel }, Cmd.map HomeMsg cmd

let update (msg: Msg) (model: Model) =
    match msg, model.CurrentPage with
    | UrlChanged url, _ ->
        handleUrlChange url model
    | LoginMsg loginMsg, Page.Login loginModel ->
        updateLoginModel loginMsg loginModel model
    | HomeMsg homeMsg, Page.Home homeModel ->
        updateHomeModel homeMsg homeModel model
    | _, _ ->
        model, Cmd.none

let getAccessDeniedPage () =
    Bulma.container [
                    Bulma.column [
                        column.is6
                        column.isOffset3
                        prop.children [
                            Bulma.title [
                                text.hasTextCentered
                                prop.text "Access denied"
                            ]
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


let getPreLoginLayout children =
    let getNavbarStart () =
        Bulma.navbarStart.div [
            Bulma.navbarItem.a [ prop.text "Contact" ]
            Bulma.navbarItem.a [ prop.text "About" ]
        ]

    let getNavbarEnd () =
        Bulma.navbarEnd.div [
            Bulma.navbarItem.div [
                Bulma.buttons [
                    Bulma.button.a [
                        prop.className "button is-info"
                        //prop.href (Router.format(""))
                        prop.children [
                            Html.strong "Register"
                        ]
                    ]
                ]
            ]
            Bulma.navbarItem.div [
                Bulma.buttons [
                    Bulma.button.a [
                        prop.className "button is-info"
                        prop.href (Router.format("login"))
                        prop.children [
                            Html.strong "Login"
                        ]
                    ]
                ]
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
                                prop.href (Router.format([]))
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
                        prop.children [
                            children
                        ]
                    ]
                    Html.div [
                        prop.style [
                            style.margin.auto
                            style.textAlign.center
                        ]
                    ]
                ]
            ]
        ]
    ]

let render (model: Model) (dispatch: Msg -> unit) =
    Html.div [
        React.router [
            router.onUrlChanged (parseUrl >> UrlChanged >> dispatch)
        ]
        match model.CurrentPage with
        | Page.Home homeState -> AfterLogin.render homeState (HomeMsg >> dispatch)
        | _ ->
            let children =
                match model.CurrentPage with
                | Page.Welcome -> Welcome.render
                | Page.Login loginState -> Login.render loginState (LoginMsg >> dispatch)
                | _ -> getAccessDeniedPage ()
            getPreLoginLayout children        
    ]

    
  