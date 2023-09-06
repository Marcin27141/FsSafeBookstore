[<RequireQualifiedAccess>]
module Booklist

open Feliz
open Feliz.Bulma
open Shared
open Fable.Remoting.Client
open Elmish
open System
open ViewUtils
open ApiProxy
open Feliz.Router

type Model = { Books: Remote<list<Book>> }

[<RequireQualifiedAccess>]
type Intent =
    | ShowBookDetails of Book
    | DoNothing

type Msg =
    | LoadBooks
    | GotBooks of Book list
    | ShowBookDetails of Book
    | DeleteBook of Book
    | BookDeleted of bool

let bookstoreApi = getApiProxy ()

let init () : Model * Cmd<Msg> =
    let model = { Books = EmptyState }
    model, Cmd.ofMsg LoadBooks

let update (msg: Msg) (model: Model) =
    match msg with
    | LoadBooks ->
        let cmd = Cmd.OfAsync.perform bookstoreApi.getBooks () GotBooks
        { model with Books = Loading }, cmd, Intent.DoNothing
    | GotBooks books ->
        let newModel = { model with Books = Body books }
        newModel, Cmd.none, Intent.DoNothing
    | ShowBookDetails book ->
        model, Cmd.none, Intent.ShowBookDetails book
    | DeleteBook book ->
        let cmd = Cmd.OfAsync.perform bookstoreApi.deleteBook book BookDeleted
        model, cmd, Intent.DoNothing
    | BookDeleted true ->
        model, Cmd.ofMsg LoadBooks, Intent.DoNothing
    | _ ->
        model, Cmd.none, Intent.DoNothing

let getCardFromBook (dispatch: Msg -> unit) (book: Book)  =
    Bulma.card [
        Bulma.cardImage [
            Bulma.image [
                Bulma.image.is4by3
                prop.children [
                    Html.img [
                        prop.alt "Placeholder image"
                        prop.src "/book3.jpg"
                    ]
                ]
            ]
        ]
        Bulma.cardContent [
            Bulma.content [
                    prop.children [
                        Bulma.title.p [
                            Bulma.title.is4
                            prop.text book.Title
                            prop.className "has-text-black-bis"
                        ]
                        Bulma.subtitle.p [
                            Bulma.title.is6
                            prop.text $"{book.Author.FirstName} {book.Author.LastName}"
                            prop.className "has-text-black-bis"
                        ]
                    ]
                ]           
        ]
        Bulma.cardFooter [
            Bulma.cardFooterItem.a [
                prop.text "Details"
                prop.onClick (fun _ -> dispatch (ShowBookDetails book))
            ]
            Bulma.cardFooterItem.a [
                prop.text "Delete"
                prop.onClick (fun _ -> dispatch (DeleteBook book))
            ]
        ]
        Html.div [ prop.className "mb-4" ]
    ]
    

let render (model: Model) (dispatch: Msg -> unit) =
    match model.Books with
    | EmptyState -> Html.none
    | Loading -> getPageLoader ()
    | LoadError e -> getErrorPageContent e
    | Body [] ->
        Html.text "No books found"
    | Body books ->
        Html.div [
            prop.children (books |> List.map (fun book -> getCardFromBook dispatch book))
        ]
