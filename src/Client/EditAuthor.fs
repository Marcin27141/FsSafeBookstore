[<RequireQualifiedAccess>]
module EditAuthor

open Feliz
open Feliz.Bulma
open Shared
open Fable.Remoting.Client
open Elmish
open System
open ViewUtils
open ApiProxy
open Feliz.Router

type Model = { OldAuthor: Author; EditedAuthor: Option<Author>; FirstNameInput: string; LastNameInput: string }

[<RequireQualifiedAccess>]
type Intent =
    | ShowEditedAuthor of Author
    | DoNothing

type Msg =
    | SetFirstNameInput of string
    | SetLastNameInput of string
    | EditAuthor
    | EditedAuthor of bool

let bookstoreApi = getApiProxy ()

let init (oldAuthor: Author) : Model * Cmd<Msg> =
    let model = { OldAuthor = oldAuthor; EditedAuthor = None; FirstNameInput = oldAuthor.FirstName; LastNameInput = oldAuthor.LastName }
    model, Cmd.none

let update (msg: Msg) (model: Model) =
    match msg with
    | SetFirstNameInput value ->
        let newModel = { model with FirstNameInput = value }
        newModel, Cmd.none, Intent.DoNothing
    | SetLastNameInput value ->
        let newModel = { model with LastNameInput = value }
        newModel, Cmd.none, Intent.DoNothing
    | EditAuthor ->
        let editedAuthor = Author.create (model.FirstNameInput, model.LastNameInput)
        let editedAuthorWithOldId = { editedAuthor with Id = model.OldAuthor.Id }
        let cmd = Cmd.OfAsync.perform bookstoreApi.editAuthor (model.OldAuthor, editedAuthorWithOldId) EditedAuthor
        { model with EditedAuthor = Some editedAuthorWithOldId }, cmd, Intent.DoNothing
    | EditedAuthor _ ->
        model, Cmd.none, Intent.ShowEditedAuthor model.EditedAuthor.Value

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
                                        prop.disabled (String.IsNullOrEmpty model.FirstNameInput || String.IsNullOrEmpty model.LastNameInput )
                                        prop.onClick (fun _ -> dispatch EditAuthor)
                                        prop.text "Edit"
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]




