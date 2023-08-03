[<RequireQualifiedAccess>]
module Index

open Elmish
open Fable.Remoting.Client
open Shared
open System
open Feliz
open Feliz.Bulma

[<RequireQualifiedAccess>]
type Page =
    | BookList of Booklist.Model
    | BookDetails of BookDetails.Model
    | CreateAuthor of CreateAuthor.Model

type Model = { CurrentPage: Page }

type Msg =
    | BookMsg of Booklist.Msg
    | AuthorMsg of CreateAuthor.Msg
    | BookDetailsMsg of BookDetails.Msg
    | SwitchToCreateAuthor
    | SwitchToBooklist
    | SwitchToBookDetails of Book

let init () : Model * Cmd<Msg> =
    let bookModel, bookCmd = Booklist.init()
    let model = { CurrentPage = Page.BookList bookModel }
    model, Cmd.map BookMsg bookCmd

let update (msg: Msg) (model:Model) :Model * Cmd<Msg> =
    match model.CurrentPage, msg with
    | Page.BookList bookModel, BookMsg bookMsg ->
        match bookMsg with
        | Booklist.Msg.ShowBookDetails book ->
            model, Cmd.ofMsg (SwitchToBookDetails book)
        | _ ->
            let bookModel, cmd = Booklist.update bookMsg bookModel
            { model with CurrentPage = Page.BookList bookModel }, Cmd.map BookMsg cmd
    | Page.CreateAuthor authorModel, AuthorMsg authorMsg ->
        let authorModel, cmd = CreateAuthor.update authorMsg authorModel
        { model with CurrentPage = Page.CreateAuthor authorModel }, Cmd.map AuthorMsg cmd
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
                            Bulma.title [
                                text.hasTextCentered
                                prop.text "FsSafeApplication"
                            ]
                            Bulma.button.a [
                                color.isInfo
                                let getMsgAndText =
                                    match model.CurrentPage with
                                    | Page.BookList _ -> SwitchToCreateAuthor, "Create author"
                                    | _ -> SwitchToBooklist, "Show booklist"
                                let msg, text = getMsgAndText
                                prop.onClick (fun _ -> dispatch msg)
                                prop.text text
                            ]
                            match model.CurrentPage with
                            | Page.BookList model -> Booklist.render model (Msg.BookMsg >> dispatch)
                            | Page.CreateAuthor model -> CreateAuthor.render model (Msg.AuthorMsg >> dispatch)
                            | Page.BookDetails model -> BookDetails.render model (Msg.BookDetailsMsg >> dispatch)
                        ]
                    ]
                ]
            ]
        ]
    ]