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
    

let render (model:Model) (dispatch: Msg -> unit) =
    Bulma.hero [
        hero.isFullHeight
        color.isPrimary
        prop.style [
            style.backgroundSize "cover"
            style.backgroundImageUrl "https://unsplash.it/1200/900?random"
            style.backgroundPosition "no-repeat center center fixed"
        ]
        prop.children [
            Bulma.heroHead [
                Bulma.navbar [
                    Bulma.container [ navBrand ]
                ]
            ]
            Bulma.heroBody [
                Bulma.container [
                    Bulma.column [
                        column.is6
                        column.isOffset3
                        prop.children [
                            //React.router [
                            //    router.onUrlChanged (parseUrl >> UrlChanged >> dispatch)
                            //]
                            Bulma.title [
                                text.hasTextCentered
                                prop.text "FsSafeApplication"
                            ]
                            match model.CurrentPage with
                            | Page.Loading -> Html.div [ Bulma.pageLoader.isActive ]
                            | Page.BookList model -> Booklist.render model (Msg.BookMsg >> dispatch)
                            | Page.CreateAuthor model -> CreateAuthor.render model (Msg.AuthorMsg >> dispatch)
                            | Page.BookDetails model -> BookDetails.render model (Msg.BookDetailsMsg >> dispatch)
                            | Page.NotFound -> Html.h1 "Page not found"
                        ]
                    ]
                    Html.div [
                        prop.style [
                            style.margin.auto
                            style.textAlign.center
                            style.width (length.percent 100)
                        ]

                        //prop.children [
                        //    Bulma.button.a [
                        //        color.isInfo
                        //        let getMsgAndText =
                        //            match model.CurrentPage with
                        //            | Page.BookList _ -> SwitchToCreateAuthor, "Create author"
                        //            | _ -> SwitchToBooklist, "Show booklist"
                        //        let msg, text = getMsgAndText
                        //        prop.onClick (fun _ -> dispatch msg)
                        //        prop.text text
                        //    ]
                        //]
                    ]
                ]
            ]
        ]
        ]