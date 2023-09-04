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

type Model = { Books: Book list; IsLoading: bool }

[<RequireQualifiedAccess>]
type Intent =
    | ShowBookDetails of Book
    | DoNothing

type Msg =
    | GotBooks of Book list
    | ShowBookDetails of Book
    | DeleteBook of Book
    | BookDeleted of bool

let bookstoreApi = getApiProxy ()

let init () : Model * Cmd<Msg> =
    let model = { Books = []; IsLoading = true }
    let initialCmd = Cmd.OfAsync.perform bookstoreApi.getBooks () GotBooks
    model, initialCmd

let update (msg: Msg) (model: Model) =
    match msg with
    | GotBooks books ->
        let newModel = { model with Books = books; IsLoading = false }
        newModel, Cmd.none, Intent.DoNothing
    | ShowBookDetails book ->
        { model with IsLoading = true }, Cmd.none, Intent.ShowBookDetails book
    | DeleteBook book ->
        let cmd = Cmd.OfAsync.perform bookstoreApi.deleteBook book BookDeleted
        { model with IsLoading = true }, cmd, Intent.DoNothing
    | BookDeleted true ->
        let cmd = Cmd.OfAsync.perform bookstoreApi.getBooks () GotBooks
        { model with IsLoading = false }, cmd, Intent.DoNothing
    | _ -> model, Cmd.none, Intent.DoNothing

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
    if model.IsLoading then
        Html.div [
            Bulma.pageLoader.isActive
            Bulma.pageLoader.isSuccess
        ]
    else
        if model.Books.Length > 0 then
            Html.div [
                prop.children (model.Books |> List.map (fun book -> getCardFromBook dispatch book))
            ]
        else
            Html.text "No books found"




