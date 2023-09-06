module ReactSpinner

open Fable.Core
open Fable.Core.JsInterop
open Fable.React

type Prop =
    | Loading of bool
    | Color of string
    | SpeedMultiplier of int
    | Height of int
    | Width of int
    | Radius of int

let reactSpinner (props : Prop list) : ReactElement =
    let propsObject = keyValueList CaseRules.LowerFirst props
    ofImport "default" "react-spinners/FadeLoader" propsObject []