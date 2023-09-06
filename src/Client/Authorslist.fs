[<RequireQualifiedAccess>]
module Authorslist

open Feliz
open Feliz.Bulma
open Shared
open Fable.Remoting.Client
open Elmish
open System
open ViewUtils
open ApiProxy
open Feliz.Router

type Model = { Authors: Author list; IsLoading: bool }

[<RequireQualifiedAccess>]
type Intent =
    | ShowAuthorDetails of Author
    | DoNothing

type Msg =
    | GotAuthors of Author list
    | ShowAuthorDetails of Author
    | DeleteAuthor of Author
    | AuthorDeleted of bool

let bookstoreApi = getApiProxy ()

let init () : Model * Cmd<Msg> =
    let model = { Authors = []; IsLoading = true }
    let initialCmd = Cmd.OfAsync.perform bookstoreApi.getAuthors () GotAuthors
    model, initialCmd

let update (msg: Msg) (model: Model) =
    match msg with
    | GotAuthors authors ->
        let newModel = { model with Authors = authors; IsLoading = false }
        newModel, Cmd.none, Intent.DoNothing
    | ShowAuthorDetails author ->
        { model with IsLoading = true }, Cmd.none, Intent.ShowAuthorDetails author
    | DeleteAuthor author ->
        let cmd = Cmd.OfAsync.perform bookstoreApi.deleteAuthor author AuthorDeleted
        { model with IsLoading = true }, cmd, Intent.DoNothing
    | AuthorDeleted true ->
        let cmd = Cmd.OfAsync.perform bookstoreApi.getAuthors () GotAuthors
        { model with IsLoading = false }, cmd, Intent.DoNothing
    | _ -> model, Cmd.none, Intent.DoNothing

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
                prop.text "Details"
                prop.onClick (fun _ -> dispatch (ShowAuthorDetails author))
            ]
            Bulma.cardFooterItem.a [
                prop.text "Delete"
                prop.onClick (fun _ -> dispatch (DeleteAuthor author))
            ]
        ]
        Html.div [ prop.className "mb-4" ]
    ]
    

let render (model: Model) (dispatch: Msg -> unit) =
    if model.IsLoading then
        getPageLoader ()
    else
        if model.Authors.Length > 0 then
            Html.div [
                prop.children (model.Authors |> List.map (fun author -> getCardFromAuthor dispatch author))
            ]
        else
            Html.text "No books found"




