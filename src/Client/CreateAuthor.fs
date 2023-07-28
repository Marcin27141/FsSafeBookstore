[<RequireQualifiedAccess>]
module CreateAuthor

open Shared
open Fable.Remoting.Client
open Elmish
open Feliz
open Feliz.Bulma

type Model = { Authors: Author list; FirstNameInput: string; LastNameInput: string }

type Msg =
    | GotAuthors of Author list
    | SetFirstNameInput of string
    | SetLastNameInput of string
    | AddAuthor
    | AddedAuthor of Author

let bookstoreApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IBookstoreApi>

let init () : Model * Cmd<Msg> =
    let model = { Authors = []; FirstNameInput = ""; LastNameInput = "" }
    let cmd = Cmd.OfAsync.perform bookstoreApi.getAuthors () GotAuthors
    model, cmd

let update (msg: Msg) (model: Model) =
    match msg with
    | GotAuthors authors ->
        let newModel = { model with Authors = authors }
        newModel, Cmd.none
    | SetFirstNameInput value ->
        let newModel = { model with FirstNameInput = value }
        newModel, Cmd.none
    | SetLastNameInput value ->
        let newModel = { model with LastNameInput = value }
        newModel, Cmd.none
    | AddAuthor ->
        let author = Author.create (model.FirstNameInput, model.LastNameInput)
        let cmd = Cmd.OfAsync.perform bookstoreApi.addAuthor author AddedAuthor
        let newModel = { model with FirstNameInput = ""; LastNameInput = "" }
        newModel, cmd 
    | AddedAuthor author ->
        let newModel = { model with Authors = model.Authors @ [ author ] }
        newModel, Cmd.none    

let render (model: Model) (dispatch: Msg -> unit) =
    Bulma.box [
        Bulma.content [
            Html.ol [
                for author in model.Authors do
                    Html.li [ prop.text (sprintf "%s %s" author.FirstName author.LastName) ]
            ]
        ]
        Bulma.field.div [
            field.isGrouped
            prop.children [
                Bulma.control.p [
                    control.isExpanded
                    prop.children [
                        Bulma.input.text [
                            prop.value model.FirstNameInput
                            prop.placeholder "Author's first name"
                            prop.onChange (fun x -> SetFirstNameInput x |> dispatch)
                        ]
                        Bulma.input.text [
                            prop.value model.LastNameInput
                            prop.placeholder "Author's last name"
                            prop.onChange (fun x -> SetLastNameInput x |> dispatch)
                        ]
                    ]
                ]
                Bulma.control.p [
                    Bulma.button.a [
                        color.isPrimary
                        prop.disabled (Author.isValid(model.FirstNameInput, model.LastNameInput) |> not)
                        prop.onClick (fun _ -> dispatch AddAuthor)
                        prop.text "Add"
                    ]
                ]
            ]
        ]
    ]