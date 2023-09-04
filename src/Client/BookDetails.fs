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
    | EditBook of Book

let init (book: Book) =
    { Book = book }, Cmd.none

let update (msg: Msg) (model: Model) =
    match msg with
    | Return -> model, Cmd.navigate("booklist", "books")
    | EditBook book -> model, Cmd.none

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
                prop.text "Edit"
                prop.onClick (fun _ -> dispatch (EditBook book))
            ]
        ]
        Html.div [ prop.className "mb-4" ]
    ]

let render (model: Model) (dispatch: Msg -> unit) =
    Html.div [
        prop.style [
            style.margin.auto
            style.textAlign.center
            style.width (length.percent 100)
        ]

        prop.children [
            //let bookTableRows =
            //    [ Html.tr [
            //            Html.td [ Html.p "Title" ]
            //            Html.td [ Html.h3 model.Book.Title ]
            //            ] ;
            //    Html.tr [
            //            Html.td [ Html.p "Author" ]
            //            Html.td [ Html.h3 (sprintf "%s %s" model.Book.Author.FirstName model.Book.Author.LastName) ]
            //            ]]
            //Bulma.box [    
            //    Bulma.content [
            //        Bulma.table bookTableRows
            //        ]
            //    ]
            getCardFromBook dispatch model.Book
            Html.div [ prop.style [style.marginTop 30 ]]
            Bulma.button.a [
                color.isInfo
                prop.text "Return"
                prop.onClick (fun _ -> dispatch Return)
            ]
        ]
    ]
    