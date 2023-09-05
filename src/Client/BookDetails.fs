[<RequireQualifiedAccess>]
module BookDetails


open Feliz
open Feliz.Bulma
open Shared
open Elmish
open System
open Feliz.Router

type Model = { Book: Book }

[<RequireQualifiedAccess>]
type Intent =
    | EditBook of Book
    | DoNothing

type Msg =
    | Return
    | EditBook of Book

let init (book: Book) =
    { Book = book }, Cmd.none

let update (msg: Msg) (model: Model) =
    match msg with
    | Return -> model, Cmd.navigate("booklist", "books"), Intent.DoNothing
    | EditBook book -> model, Cmd.none, Intent.EditBook book

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
            getCardFromBook dispatch model.Book
            Html.div [ prop.style [style.marginTop 30 ]]
            Bulma.button.a [
                color.isInfo
                prop.text "Return"
                prop.onClick (fun _ -> dispatch Return)
            ]
        ]
    ]
    