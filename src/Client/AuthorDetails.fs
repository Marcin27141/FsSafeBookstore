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
                        Html.td [ Html.p "First name" ]
                        Html.td [ Html.h3 model.Author.FirstName ]
                        ] ;
                Html.tr [
                        Html.td [ Html.p "Last name" ]
                        Html.td [ Html.h3 model.Author.LastName ]
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
    

