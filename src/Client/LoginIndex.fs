[<RequireQualifiedAccess>]
module LoginIndex

open Elmish

[<RequireQualifiedAccess>]
type Page =
  | Login of Login.Model
  | Home of AfterLogin.Model

type Model =
  { CurrentPage: Page }

type Msg =
    | LoginMsg of Login.Msg
    | HomeMsg of AfterLogin.Msg

let init() =
    let loginState, loginCmd = Login.init()
    { CurrentPage = Page.Login loginState }, Cmd.map LoginMsg loginCmd

let update (msg: Msg) (model: Model) =
    match msg, model.CurrentPage with
    | LoginMsg loginMsg, Page.Login loginModel ->
        let updatedLoginModel, loginCmd, intent = Login.update loginMsg loginModel
        match intent with
        | Login.Intent.UserLoggedIn user ->
            let homeModel, homeCmd = AfterLogin.init user
            { model with CurrentPage = Page.Home homeModel}, Cmd.map HomeMsg homeCmd
        | Login.Intent.DoNothing ->
            { model with CurrentPage = Page.Login updatedLoginModel }, Cmd.map LoginMsg loginCmd
    | HomeMsg homeMsg, Page.Home homeModel ->
        let updatedModel, cmd, intent = AfterLogin.update homeMsg homeModel
        match intent with
        | AfterLogin.Intent.LogoutUser user ->
            let loginModel, loginCmd = Login.init()
            { model with CurrentPage = Page.Login loginModel }, Cmd.map LoginMsg loginCmd
        | AfterLogin.Intent.DoNothing ->
            { model with CurrentPage = Page.Home updatedModel }, Cmd.map HomeMsg cmd
    | _, _ ->
        model, Cmd.none

let render (model: Model) (dispatch: Msg -> unit) =
  match model.CurrentPage with
  | Page.Login loginState -> Login.render loginState (LoginMsg >> dispatch)
  | Page.Home homeState -> AfterLogin.render homeState (HomeMsg >> dispatch)