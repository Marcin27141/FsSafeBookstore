[<RequireQualifiedAccess>]
module AuthorDetails

open Feliz
open Feliz.Bulma
open Shared
open Elmish
open System
open Feliz.Router

type Model = { Author: Author}

type Msg =
    | Return

let init (author: Author) =
    { Author = author}, Cmd.none

let update (msg: Msg) (model: Model) =
    match msg with
    | Return -> model, Cmd.navigate("booklist", "authors")

let getCardFromAuthor (dispatch: Msg -> unit) (author: Author)  =
    Bulma.card [
        Bulma.cardImage [
            Bulma.image [
                Bulma.image.is4by3
                prop.children [
                    Html.img [
                        prop.alt "Placeholder image"
                        prop.src "/personIcon.jpg"
                    ]
                ]
            ]
        ]
        Bulma.cardContent [
            Bulma.content [
                    prop.children [
                        Bulma.title.p [
                            Bulma.title.is4
                            prop.text $"{author.FirstName} {author.LastName}"
                            prop.className "has-text-black-bis"
                        ]
                    ]
                ]           
        ]
        Bulma.cardFooter [
            Bulma.cardFooterItem.a [
                prop.text "Edit"
                //prop.onClick (fun _ -> dispatch (EditBook book))
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
            getCardFromAuthor dispatch model.Author
            Html.div [ prop.style [style.marginTop 30 ]]
            Bulma.button.a [
                color.isInfo
                prop.text "Return"
                prop.onClick (fun _ -> dispatch Return)
            ]
        ]
    ]
    

