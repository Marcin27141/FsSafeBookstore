module ApiProxy

open Fable.Remoting.Client
open Shared

let private apiProxy =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IServerApi>

let getApiProxy () =
    apiProxy