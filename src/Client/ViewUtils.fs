module ViewUtils

open Fable.React
open Feliz
open Feliz.Bulma

let centered (children: ReactElement list) =
    Html.div [
        prop.style [
            style.margin.auto
            style.textAlign.center
            style.padding 20
            style.width (length.percent 100)
        ]

        prop.children children
    ]

let getPageNotFoundContent () =
    Bulma.title [
        text.hasTextCentered
        prop.text "Page not found"
    ]