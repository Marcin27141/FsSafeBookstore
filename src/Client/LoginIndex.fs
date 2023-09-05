[<RequireQualifiedAccess>]
module LoginIndex

open Feliz.Router
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

let getUrlString page =
    match page with
    | Page.Login _ -> "login"
    | Page.Home _ -> ""
        

let init() =
    let loginState, loginCmd = Login.init()
    let currentPage = Page.Login loginState
    let cmdBatch = Cmd.batch [ Cmd.map LoginMsg loginCmd; Cmd.navigate(getUrlString currentPage) ] 
    { CurrentPage = currentPage}, cmdBatch

let updateLoginModel loginMsg loginModel model =
    let updatedLoginModel, loginCmd, intent = Login.update loginMsg loginModel
    match intent with
    | Login.Intent.UserLoggedIn user ->
        let homeModel, homeCmd = AfterLogin.init user
        let currentPage = Page.Home homeModel
        let cmdBatch = Cmd.batch [ Cmd.map HomeMsg homeCmd; Cmd.navigate(getUrlString currentPage)]
        { model with CurrentPage = currentPage}, cmdBatch 
    | Login.Intent.DoNothing ->
        { model with CurrentPage = Page.Login updatedLoginModel }, Cmd.map LoginMsg loginCmd

let updateHomeModel homeMsg homeModel model =
    let updatedModel, cmd, intent = AfterLogin.update homeMsg homeModel
    match intent with
    | AfterLogin.Intent.LogoutUser user ->
        let loginModel, loginCmd = Login.init()
        let currentPage = Page.Login loginModel
        let cmdBatch = Cmd.batch [ Cmd.map LoginMsg loginCmd; Cmd.navigate(getUrlString currentPage)]
        { model with CurrentPage = currentPage}, cmdBatch 
    | AfterLogin.Intent.DoNothing ->
        { model with CurrentPage = Page.Home updatedModel }, Cmd.map HomeMsg cmd

let update (msg: Msg) (model: Model) =
    match msg, model.CurrentPage with
    | LoginMsg loginMsg, Page.Login loginModel ->
        updateLoginModel loginMsg loginModel model
    | HomeMsg homeMsg, Page.Home homeModel ->
        updateHomeModel homeMsg homeModel model
    | _, _ ->
        model, Cmd.none

let render (model: Model) (dispatch: Msg -> unit) =
  match model.CurrentPage with
  | Page.Login loginState -> Login.render loginState (LoginMsg >> dispatch)
  | Page.Home homeState -> AfterLogin.render homeState (HomeMsg >> dispatch)