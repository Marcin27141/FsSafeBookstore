[<RequireQualifiedAccess>]
module Welcome

open Elmish
open Shared
open Feliz
open Feliz.Router
open Fable
open ViewUtils
open System
open Feliz.Bulma
open UrlLookup

let render =
    Bulma.container [
                    Bulma.column [
                        column.is6
                        column.isOffset3
                        prop.children [
                            Bulma.title [
                                text.hasTextCentered
                                prop.text "Welcome on the website!"
                            ]
                            centered [
                                Bulma.subtitle [
                                    text.hasTextCentered
                                    prop.text "Register or log in to start"
                                ]
                            ]
                        ]
                    ]
                    Html.div [
                        prop.style [
                            style.margin.auto
                            style.textAlign.center
                            style.width (length.percent 100)
                        ]
                    ]
                ]