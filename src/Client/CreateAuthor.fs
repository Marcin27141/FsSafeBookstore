[<RequireQualifiedAccess>]
module CreateAuthor

open Shared
open Fable.Remoting.Client
open Elmish
open Feliz
open Feliz.Bulma
open ApiProxy
open Feliz.Router
open ViewUtils
open System

type Model = { FirstNameInput: string; LastNameInput: string }

type Msg =
    | SetFirstNameInput of string
    | SetLastNameInput of string
    | AddAuthor
    | AddedAuthor of Author

let bookstoreApi = getApiProxy ()

let init () : Model * Cmd<Msg> =
    let model = { FirstNameInput = ""; LastNameInput = "" }
    model, Cmd.none

let update (msg: Msg) (model: Model) =
    match msg with
    | SetFirstNameInput value ->
        let newModel = { model with FirstNameInput = value }
        newModel, Cmd.none
    | SetLastNameInput value ->
        let newModel = { model with LastNameInput = value }
        newModel, Cmd.none
    | AddAuthor ->
        let author = Author.create (model.FirstNameInput, model.LastNameInput)
        let cmd = Cmd.OfAsync.perform bookstoreApi.addAuthor author AddedAuthor
        model, cmd 
    | AddedAuthor _ ->
        model, Cmd.none

let render (model: Model) (dispatch: Msg -> unit) =
    Html.div [
        prop.children [
            Bulma.box [
                Bulma.field.div [
                    field.isGrouped
                    prop.children [
                        Bulma.control.p [
                            control.isExpanded
                            prop.children [
                                Bulma.container [
                                    Bulma.label [
                                        prop.className "label"
                                        prop.text "First Name"
                                    ]
                                    Bulma.input.text [
                                        prop.value model.FirstNameInput
                                        prop.placeholder "Author's first name"
                                        prop.onChange (fun x -> SetFirstNameInput x |> dispatch)
                                    ]
                                ]
                                Html.div [ prop.className "mb-4" ]
                                Bulma.container [
                                    Bulma.label [
                                        prop.className "label"
                                        prop.text "Last Name"
                                    ]
                                    Bulma.input.text [
                                        prop.value model.LastNameInput
                                        prop.placeholder "Author's last name"
                                        prop.onChange (fun x -> SetLastNameInput x |> dispatch)
                                    ]
                                ]
                                Html.div [ prop.className "mb-4" ]
                                centered [
                                    Bulma.button.a [
                                        color.isPrimary
                                        prop.disabled (String.IsNullOrEmpty model.FirstNameInput || String.IsNullOrEmpty model.LastNameInput)
                                        prop.onClick (fun _ -> dispatch AddAuthor)
                                        prop.text "Add"
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]
    