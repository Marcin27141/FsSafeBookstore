[<RequireQualifiedAccess>]
module Index

open Elmish
open Fable.Remoting.Client
open Shared
open System
open Feliz
open Feliz.Bulma
open Feliz.Router
open ApiProxy

[<RequireQualifiedAccess>]
type Page =
    | Loading
    | BooksIndex of BooksIndex.Model
    | AuthorsIndex of AuthorsIndex.Model
    | NotFound

[<RequireQualifiedAccess>]
type Url =
  | BooksIndexUrl of BooksIndex.Url
  | AuthorsIndexUrl of AuthorsIndex.Url
  | NotFound

let parseUrl = function
    | "books" :: booksIndexSegment -> Url.BooksIndexUrl (BooksIndex.parseUrl booksIndexSegment)
    | "authors" :: authorsIndexSegment -> Url.AuthorsIndexUrl (AuthorsIndex.parseUrl authorsIndexSegment)
    | _ -> Url.NotFound

type Model = { CurrentPage: Page; CurrentUrl: Url }

type Msg =
    | BooksIndexMsg of BooksIndex.Msg
    | AuthorsIndexMsg of AuthorsIndex.Msg

let bookstoreApi = getApiProxy ()

let init (currentUrl: Url) : Model * Cmd<Msg> =    
    match currentUrl with
    | Url.BooksIndexUrl booksIndexUrl ->
        let model, cmd = BooksIndex.init booksIndexUrl
        let newModel = { CurrentUrl = currentUrl; CurrentPage = Page.BooksIndex model}
        newModel, Cmd.map BooksIndexMsg cmd
    | Url.AuthorsIndexUrl authorsIndexUrl ->
        let model, cmd = AuthorsIndex.init authorsIndexUrl
        let newModel = { CurrentUrl = currentUrl; CurrentPage = Page.AuthorsIndex model}
        newModel, Cmd.map AuthorsIndexMsg cmd
    | Url.NotFound ->
        { CurrentUrl = currentUrl; CurrentPage = Page.NotFound }, Cmd.none

let update (msg: Msg) (model:Model) :Model * Cmd<Msg> =
    match model.CurrentPage, msg with
    | Page.BooksIndex booksIndexModel, BooksIndexMsg bookMsg ->
        let updatedModel, cmd = BooksIndex.update bookMsg booksIndexModel
        { model with CurrentPage = Page.BooksIndex updatedModel}, Cmd.map BooksIndexMsg cmd
    | Page.AuthorsIndex authorsIndexModel, AuthorsIndexMsg authorMsg ->
        let updatedModel, cmd = AuthorsIndex.update authorMsg authorsIndexModel
        { model with CurrentPage = Page.AuthorsIndex updatedModel}, Cmd.map AuthorsIndexMsg cmd   
    | _, _ -> model, Cmd.none

let render (model:Model) (dispatch: Msg -> unit) =
    Bulma.container [
        Bulma.column [
            column.is6
            column.isOffset3
            prop.children [
                match model.CurrentPage with
                | Page.Loading -> Html.div [ Bulma.pageLoader.isActive ]
                | Page.BooksIndex model -> BooksIndex.render model (Msg.BooksIndexMsg >> dispatch)
                | Page.AuthorsIndex model -> AuthorsIndex.render model (Msg.AuthorsIndexMsg >> dispatch)
                | Page.NotFound -> Html.h1 "Page not found"
            ]
        ]
    ]