module ViewUtils

open Fable.React
open Feliz
open Feliz.Bulma
open ReactSpinner

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

let getPageLoader () =
    Html.div [
        prop.style [
            style.display.flex
            style.flexDirection.column
            style.alignItems.center
            style.justifyContent.center
            style.height (length.percent 100)
        ]
        prop.children [
            reactSpinner [
                Loading true
                Color "white"
                SpeedMultiplier 1
                Height 15
                Width 5
                Radius 2
            ]
        ]
    ]