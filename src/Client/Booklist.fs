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

type Model = { Books: Book list }

[<RequireQualifiedAccess>]
type Intent =
    | ShowBookDetails of Book
    | DoNothing

type Msg =
    | GotBooks of Book list
    | ShowBookDetails of Book

let bookstoreApi = getApiProxy ()

let init () : Model * Cmd<Msg> =
    let model = { Books = [] }
    let initialCmd = Cmd.OfAsync.perform bookstoreApi.getBooks () GotBooks
    model, initialCmd

let update (msg: Msg) (model: Model) =
    match msg with
    | GotBooks books ->
        let newModel = { model with Books = books }
        newModel, Cmd.none, Intent.DoNothing
    | ShowBookDetails book ->
        model, Cmd.none, Intent.ShowBookDetails book

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
            ]
        ]
        Html.div [ prop.className "mb-4" ]
    ]
    

let render (model: Model) (dispatch: Msg -> unit) =
    Html.div [
        prop.children (model.Books |> List.map (fun book -> getCardFromBook dispatch book))
    ]




