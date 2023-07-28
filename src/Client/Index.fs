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
    | BookList
    | CreateAuthor

type Model = { AuthorModel: CreateAuthor.Model; Authors: Author list; BookModel: Booklist.Model; CurrentPage: Page }

type Msg =
    | BookMsg of Booklist.Msg
    | AuthorMsg of CreateAuthor.Msg
    | SwitchPage of Page

let bookstoreApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IBookstoreApi>

let init () : Model * Cmd<Msg> =
    let bookModel, bookCmd = Booklist.init()
    let authorModel = CreateAuthor.init()
    let model = { AuthorModel = authorModel; BookModel = bookModel; CurrentPage = Page.BookList; Authors = [] }
    model, Cmd.map BookMsg bookCmd

let updateSwitchPage (model: Model) (page: Page) =
    match page with
    | Page.BookList ->
        let newModel = { model.BookModel with Authors = model.AuthorModel.Authors }
        { model with BookModel = newModel }
    | Page.CreateAuthor -> model

let update (msg: Msg) (model:Model) :Model * Cmd<Msg> =
    match msg with
    | BookMsg bookMsg ->
        let bookModel, cmd = Booklist.update bookMsg model.BookModel
        { model with BookModel = bookModel }, Cmd.map BookMsg cmd
    | AuthorMsg authorMsg ->
        let authorModel, cmd = CreateAuthor.update authorMsg model.AuthorModel
        { model with AuthorModel = authorModel}, Cmd.map AuthorMsg cmd
    | SwitchPage page ->
        let newModel = updateSwitchPage model page
        { newModel with CurrentPage = page}, Cmd.none

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

let view (model:Model) (dispatch: Msg -> unit) =
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
                                let getPageAndText =
                                    match model.CurrentPage with
                                    | Page.BookList -> Page.CreateAuthor, "Create author"
                                    | Page.CreateAuthor -> Page.BookList, "Show booklist"
                                let page, text = getPageAndText
                                prop.onClick (fun _ -> dispatch (SwitchPage page))
                                prop.text text
                            ]
                            match model.CurrentPage with
                            | Page.BookList -> Booklist.render model.BookModel (Msg.BookMsg >> dispatch)
                            | Page.CreateAuthor -> CreateAuthor.render model.AuthorModel (Msg.AuthorMsg >> dispatch)
                        ]
                    ]
                ]
            ]
        ]
    ]