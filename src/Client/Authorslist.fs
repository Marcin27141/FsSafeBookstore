[<RequireQualifiedAccess>]
module Authorslist

open Feliz
open Feliz.Bulma
open Shared
open Fable.Remoting.Client
open Elmish
open ViewUtils
open ApiProxy

type Model = { Authors: Remote<list<Author>> }

[<RequireQualifiedAccess>]
type Intent =
    | ShowAuthorDetails of Author
    | DoNothing

type Msg =
    | LoadAuthors
    | GotAuthors of Author list
    | ShowAuthorDetails of Author
    | DeleteAuthor of Author
    | AuthorDeleted of bool

let bookstoreApi = getApiProxy ()

let init () : Model * Cmd<Msg> =
    let model = { Authors = EmptyState }
    model, Cmd.ofMsg LoadAuthors

let update (msg: Msg) (model: Model) =
    match msg with
    | LoadAuthors ->
        let updatedModel = { model with Authors = Loading }
        let cmd = Cmd.OfAsync.perform bookstoreApi.getAuthors () GotAuthors
        updatedModel, cmd, Intent.DoNothing
    | GotAuthors authors ->
        let newModel = { model with Authors = Body authors }
        newModel, Cmd.none, Intent.DoNothing
    | ShowAuthorDetails author ->
        model, Cmd.none, Intent.ShowAuthorDetails author
    | DeleteAuthor author ->
        let cmd = Cmd.OfAsync.perform bookstoreApi.deleteAuthor author AuthorDeleted
        model, cmd, Intent.DoNothing
    | AuthorDeleted true ->
        model, Cmd.ofMsg LoadAuthors, Intent.DoNothing
    | _ ->
    model, Cmd.none, Intent.DoNothing

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
    match model.Authors with
    | EmptyState -> Html.none
    | Loading -> getPageLoader ()
    | LoadError e -> getErrorPageContent e
    | Body [] ->
        Html.text "No authors found"
    | Body authors ->
        Html.div [
            prop.children (authors |> List.map (fun author -> getCardFromAuthor dispatch author))
        ]