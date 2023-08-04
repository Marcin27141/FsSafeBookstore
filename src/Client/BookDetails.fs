[<RequireQualifiedAccess>]
module BookDetails


open Feliz
open Feliz.Bulma
open Shared
open Elmish
open System
open Feliz.Router

type Model = { Book: Book }

type Msg =
    | Return

let init (book: Book) =
    { Book = book }, Cmd.none

let update (msg: Msg) (model: Model) =
    match msg with
    | Return -> model, Cmd.navigate("booklist", "books")

let render (model: Model) (dispatch: Msg -> unit) =
    Html.div [
        prop.style [
            style.margin.auto
            style.textAlign.center
            style.width (length.percent 100)
        ]

        prop.children [
            let bookTableRows =
                [ Html.tr [
                        Html.td [ Html.h1 "Title" ]
                        Html.td [ Html.text model.Book.Title ]
                        ] ;
                Html.tr [
                        Html.td [ Html.h1 "Author" ]
                        Html.td [ Html.text (sprintf "%s %s" model.Book.Author.FirstName model.Book.Author.LastName) ]
                        ]]
            Bulma.box [    
                Bulma.content [
                    Bulma.table bookTableRows
                    ]
                ]
            Html.div [ prop.style [style.marginTop 30 ]]
            Bulma.button.a [
                color.isInfo
                prop.text "Return"
                prop.onClick (fun _ -> dispatch Return)
            ]
        ]
    ]
    