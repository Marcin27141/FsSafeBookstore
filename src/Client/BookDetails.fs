[<RequireQualifiedAccess>]
module BookDetails


open Feliz
open Feliz.Bulma
open Shared
open Elmish
open System

type Model = { Book: Book }

type Msg =
    | Return

let init (book: Book) =
    { Book = book }, Cmd.none

let update (msg: Msg) (model: Model) =
    match msg with
    | Return -> model, Cmd.none

let render (model: Model) (dispatch: Msg -> unit) =
    Html.div [
        prop.style [
            style.margin.auto
            style.textAlign.center
            style.width (length.percent 100)
        ]

        prop.children [
            Html.h1 (sprintf "Title: %s" model.Book.Title)
            Html.h1 (sprintf "Author: %s %s" model.Book.Author.FirstName model.Book.Author.LastName)
        ]
    ]
    