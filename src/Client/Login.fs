[<RequireQualifiedAccess>]
module Login

open Elmish
open Feliz
open Shared
open Fable.Remoting.Client
open ApiProxy
open ViewUtils

type Model =
    { Username: string
      Password: string
      LoginProcess: LoginProcess }

type Msg =
    | UsernameChanged of string
    | PasswordChanged of string
    | LoginStarted
    | GotLoginResult of LoginResult

[<RequireQualifiedAccess>]
type Intent =
    | UserLoggedIn of User
    | DoNothing

let userApi = getApiProxy ()

let init() =
    { Username = ""
      Password = ""
      LoginProcess = LoginProcess.NotStarted }, Cmd.none

let updateOnLoginResults loginResult model =
    let nextModel = { model with LoginProcess = Finished loginResult }
    let intent =
        match loginResult with
        | LoggedIn user -> Intent.UserLoggedIn user
        | _ -> Intent.DoNothing
    nextModel, Cmd.none, intent

let sendLoginRequest model =
    let nextModel = { model with LoginProcess = InProgress }
    let cmd = async {
        let! loginResult = userApi.login ({Username = model.Username; Password = model.Password})
        return GotLoginResult loginResult
    }
    nextModel, Cmd.OfAsync.result cmd, Intent.DoNothing
    
let update (msg: Msg) (model: Model) =
    match msg with
    | UsernameChanged username ->
        { model with Username = username }, Cmd.none, Intent.DoNothing
    | PasswordChanged password ->
        { model with Password = password }, Cmd.none, Intent.DoNothing
    | LoginStarted ->
        sendLoginRequest model
    | GotLoginResult loginResult ->
        updateOnLoginResults loginResult model

let renderLoginOutcome (loginProcess: LoginProcess)=
    match loginProcess with
    | Finished UsernameOrPasswordIncorrect ->
        Html.paragraph [
            prop.style [ style.color.crimson; style.padding 10 ]
            prop.text "Username or password is incorrect"
        ]
    | Finished (LoggedIn user) ->
        Html.paragraph [
            prop.style [ style.color.green; style.padding 10 ]
            prop.text (sprintf "User '%s' has succesfully logged in" user.Username)
        ]
    | _ -> Html.none

let layout (children: ReactElement list) =
    Html.section [
        prop.className "hero is-fullheight"
        prop.style [
            style.backgroundImageUrl "/bookshelf.jpg"
            style.backgroundSize "cover"
            style.backgroundPosition "no-repeat center center fixed"
        ]
        prop.children [
            Html.div [
                prop.className "hero-body"
                prop.children [
                    Html.div [
                        prop.className "container"
                        prop.children [
                            Html.div [
                                prop.className "columns is-centered"
                                prop.children [
                                    Html.div [
                                        prop.className "column is-6-tablet is-4-desktop is-4-widescreen"
                                        prop.children children
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]

let render (model: Model) (dispatch: Msg -> unit) =
    layout [
        Html.div [
            prop.className "box"
            prop.children [

                centered [
                    Html.img [
                        prop.src "/logo_fable.png"
                        prop.height 160
                        prop.width 140
                   ]
                ]

                Html.div [
                    prop.className "field"
                    prop.children [
                        Html.label [
                            prop.className "label"
                            prop.text "Username"
                        ]

                        Html.div [
                            prop.className "control has-icons-left"
                            prop.children [
                                Html.input [
                                    prop.className "input"
                                    prop.placeholder "Username"
                                    prop.type'.email
                                    prop.valueOrDefault model.Username
                                    prop.onChange (UsernameChanged >> dispatch)
                                ]

                                Html.span [
                                    prop.className "icon is-small is-left"
                                    prop.children [
                                        Html.i [ prop.className "fa fa-user" ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]

                Html.div [
                    prop.className "field"
                    prop.children [
                        Html.label [
                            prop.className "label"
                            prop.text "Password"
                        ]
                        Html.div [
                            prop.className "control has-icons-left"
                            prop.children [
                                Html.input [
                                    prop.className "input"
                                    prop.placeholder "********"
                                    prop.type'.password
                                    prop.valueOrDefault model.Password
                                    prop.onChange (PasswordChanged >> dispatch)
                                ]
                                Html.span [
                                    prop.className "icon is-small is-left"
                                    prop.children [
                                        Html.i [ prop.className "fa fa-lock" ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]

                Html.div [
                    prop.className "field"
                    prop.children [
                        Html.button [
                            prop.className [
                                "button is-info is-fullwidth"
                                if model.LoginProcess = InProgress
                                then "is-loading"
                            ]

                            prop.onClick (fun _ -> dispatch LoginStarted)
                            prop.text "Login"
                        ]
                    ]
                ]

                renderLoginOutcome model.LoginProcess
            ]
        ]
    ]
