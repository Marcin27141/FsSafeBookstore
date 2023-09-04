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
            ]
        ]
    ]




