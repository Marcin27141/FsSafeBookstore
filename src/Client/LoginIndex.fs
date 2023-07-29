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
        match loginMsg with
        | Login.UserLoggedIn user ->
            let homeModel, homeCmd = AfterLogin.init user
            { model with CurrentPage = Page.Home homeModel}, Cmd.map HomeMsg homeCmd
        | _ ->
            let (updatedLoginModel, loginCmd) = Login.update loginMsg loginModel
            { model with CurrentPage = Page.Login updatedLoginModel}, Cmd.map LoginMsg loginCmd
    | HomeMsg homeMsg, Page.Home homeModel ->
        match homeMsg with
        | AfterLogin.Msg.Logout ->
            let loginModel, loginCmd = Login.init()
            { model with CurrentPage = Page.Login loginModel }, Cmd.map LoginMsg loginCmd
        | _ ->
            let (updatedHomeModel, homeCmd) = AfterLogin.update homeMsg homeModel
            { model with CurrentPage = Page.Home updatedHomeModel}, Cmd.map HomeMsg homeCmd
    | _, _ ->
        model, Cmd.none

let render (model: Model) (dispatch: Msg -> unit) =
  match model.CurrentPage with
  | Page.Login loginState -> Login.render loginState (LoginMsg >> dispatch)
  | Page.Home homeState -> AfterLogin.render homeState (HomeMsg >> dispatch)