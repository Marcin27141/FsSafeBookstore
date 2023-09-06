[<RequireQualifiedAccess>]
module Register

open Elmish
open Feliz
open Feliz.Bulma
open Shared
open Fable.Remoting.Client
open ApiProxy
open ViewUtils

type Model =
    { Username: string
      Password: string
      PasswordConfirm: string
      FirstName: string
      LastName: string
      RegistrationProcess: RegistrationProcess }

type Msg =
    | UsernameChanged of string
    | PasswordChanged of string
    | PasswordConfirmChanged of string
    | FirstNameChanged of string
    | LastNameChanged of string
    | CheckPasswordsEquality
    | PasswordsAreNotEqual
    | RegistrationStarted
    | GotRegistrationResult of RegistrationResult

[<RequireQualifiedAccess>]
type Intent =
    | UserRegistered of User
    | DoNothing

let userApi = getApiProxy ()

let init() =
    { Username = ""
      Password = ""
      PasswordConfirm = ""
      FirstName = ""
      LastName = ""
      RegistrationProcess = RegistrationProcess.NotStarted }, Cmd.none

let updateOnRegistrationResults registrationResult model =
    let nextModel = { model with RegistrationProcess = RegistrationProcess.Finished registrationResult }
    let intent =
        match registrationResult with
        | Registered user -> Intent.UserRegistered user
        | _ -> Intent.DoNothing
    nextModel, Cmd.none, intent

let sendRegistrationRequest model =
    let nextModel = { model with RegistrationProcess = RegistrationProcess.InProgress }
    let cmd = async {
        let! registrationResult = userApi.register ({Username = model.Username; Password = model.Password; FirstName = model.FirstName; LastName = model.LastName})
        return GotRegistrationResult registrationResult
    }
    nextModel, Cmd.OfAsync.result cmd, Intent.DoNothing
    
let update (msg: Msg) (model: Model) =
    match msg with
    | UsernameChanged username ->
        { model with Username = username }, Cmd.none, Intent.DoNothing
    | PasswordChanged password ->
        { model with Password = password }, Cmd.none, Intent.DoNothing
    | PasswordConfirmChanged passwordConfirm ->
        { model with PasswordConfirm = passwordConfirm}, Cmd.none, Intent.DoNothing
    | FirstNameChanged firstName ->
        { model with FirstName = firstName}, Cmd.none, Intent.DoNothing
    | LastNameChanged lastName ->
        { model with LastName = lastName}, Cmd.none, Intent.DoNothing
    | CheckPasswordsEquality ->
        match model.Password = model.PasswordConfirm with
        | true -> model, Cmd.ofMsg RegistrationStarted, Intent.DoNothing
        | false -> model, Cmd.ofMsg PasswordsAreNotEqual, Intent.DoNothing
    | PasswordsAreNotEqual ->
        { model with RegistrationProcess = RegistrationProcess.Finished (RegistrationResult.RegistrationError "Passwords must be identical")}, Cmd.none, Intent.DoNothing
    | RegistrationStarted ->
        sendRegistrationRequest model
    | GotRegistrationResult registrationResult ->
        updateOnRegistrationResults registrationResult model

let renderRegistrationOutcome (registrationProcess: RegistrationProcess) =
    match registrationProcess with
    | RegistrationProcess.Finished (RegistrationError error) ->
        Html.paragraph [
            prop.style [ style.color.crimson; style.padding 10 ]
            prop.text error
        ]
    | RegistrationProcess.Finished (Registered user) ->
        Html.paragraph [
            prop.style [ style.color.green; style.padding 10 ]
            prop.text (sprintf "User '%s' has been succesfully registered" user.Username)
        ]
    | _ -> Html.none

let layout (children: ReactElement list) =
    Bulma.container [
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
                            prop.text "First name"
                        ]

                        Html.div [
                            Html.input [
                                    prop.className "input"
                                    prop.placeholder "First name"
                                    prop.valueOrDefault model.FirstName
                                    prop.onChange (FirstNameChanged >> dispatch)
                                ]
                        ]
                    ]
                ]

                Html.div [
                    prop.className "field"
                    prop.children [
                        Html.label [
                            prop.className "label"
                            prop.text "Last name"
                        ]

                        Html.div [
                            Html.input [
                                    prop.className "input"
                                    prop.placeholder "Last name"
                                    prop.valueOrDefault model.LastName
                                    prop.onChange (LastNameChanged >> dispatch)
                                ]
                        ]
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
                            Html.input [
                                    prop.className "input"
                                    prop.placeholder "Username"
                                    prop.type'.email
                                    prop.valueOrDefault model.Username
                                    prop.onChange (UsernameChanged >> dispatch)
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
                            Html.input [
                                    prop.className "input"
                                    prop.placeholder "********"
                                    prop.type'.password
                                    prop.valueOrDefault model.Password
                                    prop.onChange (PasswordChanged >> dispatch)
                                ]
                        ]
                    ]
                ]

                Html.div [
                    prop.className "field"
                    prop.children [
                        Html.label [
                            prop.className "label"
                            prop.text "Confirm password"
                        ]
                        Html.div [
                            Html.input [
                                    prop.className "input"
                                    prop.placeholder "********"
                                    prop.type'.password
                                    prop.valueOrDefault model.PasswordConfirm
                                    prop.onChange (PasswordConfirmChanged >> dispatch)
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
                                if model.RegistrationProcess = RegistrationProcess.InProgress
                                then "is-loading"
                            ]

                            prop.onClick (fun _ -> dispatch CheckPasswordsEquality)
                            prop.text "Register"
                        ]
                    ]
                ]

                renderRegistrationOutcome model.RegistrationProcess
            ]
        ]
    ]
