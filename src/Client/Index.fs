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
    | BookList of Booklist.Model
    | BookDetails of BookDetails.Model
    | CreateAuthor of CreateAuthor.Model

type Model = { RouterPath: string list; CurrentPage: Page; CurrentUrl: string list }

type Msg =
    | BookMsg of Booklist.Msg
    | AuthorMsg of CreateAuthor.Msg
    | BookDetailsMsg of BookDetails.Msg
    | SwitchToCreateAuthor
    | SwitchToBooklist
    | SwitchToBookDetails of Book
    | UrlChanged of string list
    | GotBookForDetails of Book

let bookstoreApi = getApiProxy ()

let init () : Model * Cmd<Msg> =
    let bookModel, bookCmd = Booklist.init()
    let model = { CurrentPage = Page.BookList bookModel; CurrentUrl = Router.currentUrl(); RouterPath = Router.currentUrl() }
    model, Cmd.map BookMsg bookCmd

let update (msg: Msg) (model:Model) :Model * Cmd<Msg> =
    match model.CurrentPage, msg with
    | _, UrlChanged url ->
        Console.WriteLine($"inner: {url}")
        let getModelWithPageAndCmd (model : Model) =
            match url with
            | ["booklist"; "books"] ->
                let booklistModel, cmd = Booklist.init ()
                { model with CurrentPage = Page.BookList booklistModel}, Cmd.map BookMsg cmd
            | ["booklist"; "authors"] ->
                let authorsModel, cmd = CreateAuthor.init ()
                { model with CurrentPage = Page.CreateAuthor authorsModel }, Cmd.map AuthorMsg cmd
            | [ "booklist"; "details"; Route.Guid guid ] ->
                let cmd = async {
                    let! book = bookstoreApi.getBook guid
                    return GotBookForDetails book
                }
                model, Cmd.OfAsync.result cmd
            | _ -> model, Cmd.none
        getModelWithPageAndCmd { model with CurrentUrl = url }
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
    | _, SwitchToBooklist ->
        let newModel, cmd = Booklist.init()
        { model with CurrentPage = Page.BookList newModel }, Cmd.map BookMsg cmd
    | _, SwitchToCreateAuthor ->
        let newModel, cmd = CreateAuthor.init()
        { model with CurrentPage = Page.CreateAuthor newModel }, Cmd.map AuthorMsg cmd
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
                            React.router [
                                router.onUrlChanged (UrlChanged >> dispatch)
                            ]
                            Bulma.title [
                                text.hasTextCentered
                                prop.text "FsSafeApplication"
                            ]
                            match model.CurrentPage with
                            | Page.BookList model -> Booklist.render model (Msg.BookMsg >> dispatch)
                            | Page.CreateAuthor model -> CreateAuthor.render model (Msg.AuthorMsg >> dispatch)
                            | Page.BookDetails model -> BookDetails.render model (Msg.BookDetailsMsg >> dispatch)
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