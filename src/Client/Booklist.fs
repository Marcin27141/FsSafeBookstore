[<RequireQualifiedAccess>]
module Booklist

open Feliz
open Feliz.Bulma
open Shared
open Fable.Remoting.Client
open Elmish
open System

type Model = { Books: Book list; Authors: Author list; TitleInput: string; AuthorId: string }

type Msg =
    | GotBooks of Book list
    | SetTitleInput of string
    | SetAuthorId of string
    | AddBook of Author
    | AddedBook of Book
    | GetAuthor

let bookstoreApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IBookstoreApi>

let init () : Model * Cmd<Msg> =
    let model = { Books = []; Authors = []; TitleInput = ""; AuthorId = "" }
    let cmd = Cmd.OfAsync.perform bookstoreApi.getBooks () GotBooks
    model, cmd

let update (msg: Msg) (model: Model) =
    match msg with
    | GotBooks books ->
        let newModel = { model with Books = books }
        newModel, Cmd.none
    | SetTitleInput value ->
        let newModel = { model with TitleInput = value }
        newModel, Cmd.none
    | SetAuthorId value ->
        let newModel = { model with AuthorId = value }
        newModel, Cmd.none
    | GetAuthor ->
        let cmd = Cmd.OfAsync.perform bookstoreApi.getAuthor (Guid.Parse(model.AuthorId)) AddBook
        let newModel = { model with AuthorId = "" }
        newModel, cmd 
    | AddBook author ->
        let book = Book.create (model.TitleInput, author)
        let cmd = Cmd.OfAsync.perform bookstoreApi.addBook book AddedBook
        let newModel = { model with TitleInput = "" }
        newModel, cmd 
    | AddedBook book ->
        let newModel = { model with Books = model.Books @ [ book ] }
        newModel, Cmd.none


let authorDropdownOptions (authors: Author list) =
    authors
    |> List.map (fun author ->
        Html.option [
            prop.value (author.Id.ToString())
            prop.text (sprintf "%s %s" author.FirstName author.LastName)
        ])

let render (model: Model) (dispatch: Msg -> unit) =
    Bulma.box [
        Bulma.content [
            Html.ol [
                for book in model.Books do
                    Html.li [ prop.text book.Title ]
            ]
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
                        //SharedView.divider
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