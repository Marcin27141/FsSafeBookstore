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
    | AuthorDetails of AuthorDetails.Model
    | CreateAuthor of CreateAuthor.Model
    | NotFound

[<RequireQualifiedAccess>]
type Url =
  | BooksIndexUrl of BooksIndex.Url
  | CreateAuthor
  | AuthorDetails of Guid
  | NotFound

let parseUrl = function
    | "books" :: booksIndexSegment -> Url.BooksIndexUrl (BooksIndex.parseUrl booksIndexSegment)
    | [ "create"; "authors" ] -> Url.CreateAuthor
    | [ "author"; Route.Guid authorId ] -> Url.AuthorDetails authorId
    | _ -> Url.NotFound

type Model = { CurrentPage: Page; CurrentUrl: Url }

type Msg =
    | BooksIndexMsg of BooksIndex.Msg
    | CreateAuthorMsg of CreateAuthor.Msg
    | AuthorDetailsMsg of AuthorDetails.Msg
    | SwitchToAuthorDetails of Author
    | GotAuthorForDetails of Author

let bookstoreApi = getApiProxy ()

let init (currentUrl: Url) : Model * Cmd<Msg> =    
    match currentUrl with
    | Url.BooksIndexUrl booksIndexUrl ->
        let model, cmd = BooksIndex.init booksIndexUrl
        let newModel = { CurrentUrl = currentUrl; CurrentPage = Page.BooksIndex model}
        newModel, Cmd.map BooksIndexMsg cmd
    | Url.CreateAuthor ->
        let model, cmd = CreateAuthor.init ()
        let newModel = { CurrentUrl = currentUrl; CurrentPage = Page.CreateAuthor model}
        newModel, Cmd.map CreateAuthorMsg cmd
    | Url.AuthorDetails guid ->
        let cmd = async {
            let! author = bookstoreApi.getAuthor guid
            return GotAuthorForDetails author
        }
        { CurrentUrl = currentUrl; CurrentPage = Page.Loading }, Cmd.OfAsync.result cmd
    | Url.NotFound ->
        { CurrentUrl = currentUrl; CurrentPage = Page.NotFound }, Cmd.none

let update (msg: Msg) (model:Model) :Model * Cmd<Msg> =
    match model.CurrentPage, msg with
    | _, GotAuthorForDetails author ->
        let detailsModel, cmd = AuthorDetails.init author
        { model with CurrentPage = Page.AuthorDetails detailsModel}, Cmd.map AuthorDetailsMsg cmd 
    | Page.BooksIndex booksIndexModel, BooksIndexMsg bookMsg ->
        let updatedModel, cmd = BooksIndex.update bookMsg booksIndexModel
        { model with CurrentPage = Page.BooksIndex updatedModel}, Cmd.map BooksIndexMsg cmd   
    | Page.CreateAuthor authorModel, CreateAuthorMsg authorMsg ->
        match authorMsg with
        | CreateAuthor.Msg.AddedAuthor author -> model, Cmd.ofMsg (SwitchToAuthorDetails author)
        | _ ->
            let newAuthorModel, cmd = CreateAuthor.update authorMsg authorModel
            { model with CurrentPage = Page.CreateAuthor newAuthorModel }, Cmd.map CreateAuthorMsg cmd   
    | Page.AuthorDetails detailsModel, AuthorDetailsMsg detailsMsg ->
        let updatedDetailsModel, cmd = AuthorDetails.update detailsMsg detailsModel
        { model with CurrentPage = Page.AuthorDetails updatedDetailsModel}, Cmd.map AuthorDetailsMsg cmd
    | _, SwitchToAuthorDetails author ->
        let newModel, cmd = AuthorDetails.init author
        { model with CurrentPage = Page.AuthorDetails newModel }, Cmd.map AuthorDetailsMsg cmd
    | _, _ -> model, Cmd.none

let navBrand =
    Bulma.navbarBrand.div [
        Bulma.navbarItem.a [
            prop.href "https://safe-stack.github.io/"
            navbarItem.isActive
            prop.children [
                Html.img [
                    prop.src "/favicon.png"
                    prop.alt "Logo"
                ]
            ]
        ]
    ]

let centered (children: ReactElement list) =
    Html.div [
        prop.style [
            style.margin.auto
            style.textAlign.center
            style.width (length.percent 100)
        ]

        prop.children children
    ]  

let render (model:Model) (dispatch: Msg -> unit) =
    Bulma.container [
        Bulma.column [
            column.is6
            column.isOffset3
            prop.children [
                match model.CurrentPage with
                | Page.Loading -> Html.div [ Bulma.pageLoader.isActive ]
                | Page.BooksIndex model -> BooksIndex.render model (Msg.BooksIndexMsg >> dispatch)
                | Page.CreateAuthor model -> CreateAuthor.render model (Msg.CreateAuthorMsg >> dispatch)
                | Page.AuthorDetails model -> AuthorDetails.render model (Msg.AuthorDetailsMsg >> dispatch)
                | Page.NotFound -> Html.h1 "Page not found"
            ]
        ]
    ]