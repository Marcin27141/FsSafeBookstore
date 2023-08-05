[<RequireQualifiedAccess>]
module Booklist

open Feliz
open Feliz.Bulma
open Shared
open Fable.Remoting.Client
open Elmish
open System
open ApiProxy
open Feliz.Router

type Model = { Books: Book list; Authors: Author list; TitleInput: string; AuthorId: string }

[<RequireQualifiedAccess>]
type Intent =
    | ShowBookDetails of Book
    | DoNothing

type Msg =
    | GotBooks of Book list
    | GotAuthors of Author list
    | SetTitleInput of string
    | SetAuthorId of string
    | AddBook of Author
    | AddedBook of Book
    | GetAuthor
    | ShowBookDetails of Book
    | CreateAuthor

let bookstoreApi = getApiProxy ()

let init () : Model * Cmd<Msg> =
    let model = { Books = []; Authors = []; TitleInput = ""; AuthorId = "" }
    let initialCmd = Cmd.batch [
        Cmd.OfAsync.perform bookstoreApi.getBooks () GotBooks
        Cmd.OfAsync.perform bookstoreApi.getAuthors () GotAuthors
    ]
    model, initialCmd

let update (msg: Msg) (model: Model) =
    match msg with
    | GotAuthors authors ->
        let newModel = { model with Authors = authors }
        newModel, Cmd.none, Intent.DoNothing
    | GotBooks books ->
        let newModel = { model with Books = books }
        newModel, Cmd.none, Intent.DoNothing
    | SetTitleInput value ->
        let newModel = { model with TitleInput = value }
        newModel, Cmd.none, Intent.DoNothing
    | SetAuthorId value ->
        let newModel = { model with AuthorId = value }
        newModel, Cmd.none, Intent.DoNothing
    | GetAuthor ->
        let cmd = Cmd.OfAsync.perform bookstoreApi.getAuthor (Guid.Parse(model.AuthorId)) AddBook
        let newModel = { model with AuthorId = "" }
        newModel, cmd , Intent.DoNothing
    | AddBook author ->
        let book = Book.create (model.TitleInput, author)
        let cmd = Cmd.OfAsync.perform bookstoreApi.addBook book AddedBook
        let newModel = { model with TitleInput = "" }
        newModel, cmd, Intent.DoNothing
    | AddedBook book ->
        let newModel = { model with Books = model.Books @ [ book ] }
        newModel, Cmd.none, Intent.DoNothing
    | ShowBookDetails book ->
        model, Cmd.none, Intent.ShowBookDetails book
    | CreateAuthor ->
        model, Cmd.navigate("booklist", "authors"), Intent.DoNothing

let authorDropdownOptions (authors: Author list) =
    authors
    |> List.map (fun author ->
        Html.option [
            prop.value (author.Id.ToString())
            prop.text (sprintf "%s %s" author.FirstName author.LastName)
        ])



let render (model: Model) (dispatch: Msg -> unit) =
    Html.div [
        prop.children [
            Bulma.box [
                Bulma.content [
                    let booksTable =
                        [ for book in model.Books -> 
                            Html.tr [
                                Html.td [ Html.text book.Title ]
                                Html.td [
                                    Html.button [
                                        prop.text "Show"
                                        prop.onClick (fun _ -> dispatch (ShowBookDetails book))
                                    ]
                                ]
                            ]
                        ]
                    Bulma.table booksTable
                ]
                Bulma.field.div [
                    field.isGrouped
                    prop.children [
                        Bulma.control.p [
                            control.isExpanded
                            prop.children [
                                Bulma.input.text [
                                    prop.value model.TitleInput
                                    prop.placeholder "Specify book title"
                                    prop.onChange (fun x -> SetTitleInput x |> dispatch)
                                ]
                                Bulma.select [
                                    prop.onChange (fun id -> SetAuthorId id |> dispatch)
                                    prop.children (authorDropdownOptions model.Authors)
                                ]
                            ]
                        ]
                        Bulma.control.p [
                            Bulma.button.a [
                                color.isPrimary
                                prop.disabled (String.IsNullOrEmpty model.TitleInput || String.IsNullOrEmpty model.AuthorId)
                                prop.onClick (fun _ -> dispatch GetAuthor)
                                prop.text "Add"
                            ]
                        ]
                    ]
                ]
            ]
            Html.div [
                prop.style [
                    style.margin.auto
                    style.textAlign.center
                    style.width (length.percent 100)
                    style.marginTop 30
                ]
                prop.children [
                    Bulma.button.a [
                        color.isInfo
                        prop.text "Create author"
                        prop.onClick (fun _ -> dispatch CreateAuthor)
                    ]
                ]
            ]
        ]
    ]




