[<RequireQualifiedAccess>]
module EditBook

open Feliz
open Feliz.Bulma
open Shared
open Fable.Remoting.Client
open Elmish
open System
open ViewUtils
open ApiProxy
open Feliz.Router

type Model = { OldBook: Book; EditedBook: Option<Book>; Authors: Author list; TitleInput: string; AuthorId: string }

type Msg =
    | GotAuthors of Author list
    | SetTitleInput of string
    | SetAuthorId of string
    | EditBook of Author
    | EditedBook of bool
    | GetAuthor

let bookstoreApi = getApiProxy ()

let init (oldBook: Book) : Model * Cmd<Msg> =
    let model = { OldBook = oldBook; EditedBook = None; Authors = []; TitleInput = oldBook.Title; AuthorId = oldBook.Author.Id.ToString()}
    let initialCmd = Cmd.OfAsync.perform bookstoreApi.getAuthors () GotAuthors
    model, initialCmd

let update (msg: Msg) (model: Model) =
    match msg with
    | GotAuthors authors ->
        let newModel = { model with Authors = authors }
        newModel, Cmd.none
    | SetTitleInput value ->
        let newModel = { model with TitleInput = value }
        newModel, Cmd.none
    | SetAuthorId value ->
        let newModel = { model with AuthorId = value }
        newModel, Cmd.none
    | GetAuthor ->
        let cmd = Cmd.OfAsync.perform bookstoreApi.getAuthor (Guid.Parse(model.AuthorId)) EditBook
        model, cmd
    | EditBook author ->
        let editedBook = Book.create (model.TitleInput, author)
        let editedBookWithOldId = { editedBook with Id = model.OldBook.Id }
        let cmd = Cmd.OfAsync.perform bookstoreApi.editBook (model.OldBook, editedBookWithOldId) EditedBook
        { model with EditedBook = Some editedBookWithOldId }, cmd
    | EditedBook bool ->
        model, Cmd.none

let authorDropdownOptions (authors: Author list) =
    let placeholder =
         Html.option [
                prop.value ""
                prop.text "Select an author"
            ]
    authors
    |> List.map (fun author ->
        Html.option [
            prop.value (author.Id.ToString())
            prop.text (sprintf "%s %s" author.FirstName author.LastName)
        ])
    |> List.insertAt 0 placeholder



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
                                        prop.text "Title"
                                    ]
                                    Bulma.input.text [
                                        prop.value model.TitleInput
                                        prop.placeholder "Specify book title"
                                        prop.onChange (fun x -> SetTitleInput x |> dispatch)
                                    ]
                                ]
                                Html.div [ prop.className "mb-4" ]
                                Bulma.container [
                                    Bulma.label [
                                        prop.className "label"
                                        prop.text "Author"
                                    ]
                                    Bulma.select [
                                        prop.value model.AuthorId
                                        prop.onChange (fun id -> SetAuthorId id |> dispatch)
                                        prop.children (authorDropdownOptions model.Authors)
                                    ]
                                ]
                                Html.div [ prop.className "mb-4" ]
                                centered [
                                    Bulma.button.a [
                                        color.isPrimary
                                        prop.disabled (String.IsNullOrEmpty model.TitleInput || String.IsNullOrEmpty model.AuthorId)
                                        prop.onClick (fun _ -> dispatch GetAuthor)
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




