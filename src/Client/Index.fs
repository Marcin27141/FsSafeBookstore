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
    | BookList of Booklist.Model
    | BookDetails of BookDetails.Model
    | CreateAuthor of CreateAuthor.Model
    | NotFound

[<RequireQualifiedAccess>]
type Url =
  | Booklist
  | CreateAuthor
  | BookDetails of Guid
  | NotFound

let parseUrl = function
    | [ "books" ] -> Url.Booklist
    | [ "authors" ] -> Url.CreateAuthor
    | [ "details"; Route.Guid bookId ] -> Url.BookDetails bookId
    | _ -> Url.NotFound

type Model = { CurrentPage: Page; CurrentUrl: Url }

type Msg =
    | BookMsg of Booklist.Msg
    | AuthorMsg of CreateAuthor.Msg
    | BookDetailsMsg of BookDetails.Msg
    | GotBookForInit of Book
    | SwitchToCreateAuthor
    | SwitchToBooklist
    | SwitchToBookDetails of Book
    | GotBookForDetails of Book

let bookstoreApi = getApiProxy ()

let init (currentUrl: Url) : Model * Cmd<Msg> =
    match currentUrl with
    | Url.Booklist ->
        let model, cmd = Booklist.init ()
        let newModel = { CurrentUrl = currentUrl; CurrentPage = Page.BookList model}
        newModel, Cmd.map BookMsg cmd
    | Url.CreateAuthor ->
        let model, cmd = CreateAuthor.init ()
        let newModel = { CurrentUrl = currentUrl; CurrentPage = Page.CreateAuthor model}
        newModel, Cmd.map AuthorMsg cmd
    | Url.BookDetails guid ->
        let cmd = async {
            let! book = bookstoreApi.getBook guid
            return GotBookForDetails book
        }
        { CurrentUrl = currentUrl; CurrentPage = Page.Loading }, Cmd.OfAsync.result cmd
    | Url.NotFound ->
        { CurrentUrl = currentUrl; CurrentPage = Page.NotFound }, Cmd.none

let update (msg: Msg) (model:Model) :Model * Cmd<Msg> =
    match model.CurrentPage, msg with
    | _, GotBookForDetails book ->
        let detailsModel, cmd = BookDetails.init book
        { model with CurrentPage = Page.BookDetails detailsModel}, Cmd.map BookDetailsMsg cmd 
    | Page.BookList bookModel, BookMsg bookMsg ->
        let updatedModel, cmd, intent = Booklist.update bookMsg bookModel
        match intent with
        | Booklist.Intent.ShowBookDetails book ->
            { model with CurrentPage = Page.BookList updatedModel }, Cmd.ofMsg (SwitchToBookDetails book)
        | Booklist.Intent.DoNothing ->
            { model with CurrentPage = Page.BookList updatedModel }, Cmd.map BookMsg cmd
    | Page.CreateAuthor authorModel, AuthorMsg authorMsg ->
        let authorModel, cmd = CreateAuthor.update authorMsg authorModel
        { model with CurrentPage = Page.CreateAuthor authorModel }, Cmd.map AuthorMsg cmd
    | Page.BookDetails detailsModel, BookDetailsMsg detailsMsg ->
        let updatedDetailsModel, cmd = BookDetails.update detailsMsg detailsModel
        { model with CurrentPage = Page.BookDetails updatedDetailsModel}, Cmd.map BookDetailsMsg cmd
    | _, SwitchToBookDetails book ->
        let newModel, cmd = BookDetails.init book
        { model with CurrentPage = Page.BookDetails newModel }, Cmd.map BookDetailsMsg cmd
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

let getNavbar () =
    Bulma.navbarMenu [
        Bulma.navbarStart.div [
            Bulma.navbarItem.a [ prop.text "Home" ]
            Bulma.navbarItem.a [ prop.text "Documentation" ]
            Bulma.navbarItem.div [
                Bulma.navbarItem.hasDropdown
                Bulma.navbarItem.isHoverable
                prop.children [
                    Bulma.navbarLink.a [ prop.text "More" ]
                    Bulma.navbarDropdown.div [
                        Bulma.navbarItem.a [ prop.text "About" ]
                        Bulma.navbarItem.a [ prop.text "Jobs" ]
                        Bulma.navbarItem.a [ prop.text "Contact" ]
                        Bulma.navbarDivider []
                        Bulma.navbarItem.a [ prop.text "Report a issue" ]
                    ]
                ]
            ]
        ]
        Bulma.navbarEnd.div [
            Bulma.navbarItem.div [
                Bulma.buttons [
                    Bulma.button.a [
                        Bulma.color.isPrimary
                        prop.children [
                            Html.strong "Sign up"
                        ]
                    ]
                    Bulma.button.a [ prop.text "Log In" ]
                ]
            ]
        ]
    ]
    

let render (model:Model) (dispatch: Msg -> unit) =
    Bulma.container [
        Bulma.column [
            column.is6
            column.isOffset3
            prop.children [
                match model.CurrentPage with
                | Page.Loading -> Html.div [ Bulma.pageLoader.isActive ]
                | Page.BookList model -> Booklist.render model (Msg.BookMsg >> dispatch)
                | Page.CreateAuthor model -> CreateAuthor.render model (Msg.AuthorMsg >> dispatch)
                | Page.BookDetails model -> BookDetails.render model (Msg.BookDetailsMsg >> dispatch)
                | Page.NotFound -> Html.h1 "Page not found"
            ]
        ]
    ]