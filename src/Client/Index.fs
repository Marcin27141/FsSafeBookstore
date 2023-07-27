module Index

open Elmish
open Fable.Remoting.Client
open Shared
open System

type Page =
    | BookList
    | CreateAuthor

type AuthorModel = { Authors: Author list; FirstNameInput: string; LastNameInput: string }
type BookModel = { Books: Book list; Authors: Author list; TitleInput: string; AuthorId: string }
type Model = { AuthorModel: AuthorModel; Authors: Author list; BookModel: BookModel; CurrentPage: Page }

type BookMsg =
    | GotBooks of Book list
    | SetTitleInput of string
    | SetAuthorId of string
    | AddBook of Author
    | AddedBook of Book
    | GetAuthor

type AuthorMsg =
    | SetFirstNameInput of string
    | SetLastNameInput of string
    | AddAuthor
    | AddedAuthor of Author

type Msg =
    | BookMsg of BookMsg
    | AuthorMsg of AuthorMsg
    | SwitchPage of Page

let bookstoreApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IBookstoreApi>

let init () : Model * Cmd<Msg> =
    let authorModel = { Authors = []; FirstNameInput = ""; LastNameInput = "" }
    let bookModel = { Books = []; Authors = []; TitleInput = ""; AuthorId = "" }
    let model = { AuthorModel = authorModel; BookModel = bookModel; CurrentPage = Page.BookList; Authors = [] }

    let cmd = Cmd.OfAsync.perform bookstoreApi.getBooks () GotBooks

    model, Cmd.map BookMsg cmd

let update (msg: Msg) (model:Model) :Model * Cmd<Msg> =
    match msg with
    | BookMsg bookMsg ->
        match bookMsg with
        | GotBooks books ->
            let newModel = { model.BookModel with Books = books }
            { model with BookModel = newModel }, Cmd.none
        | SetTitleInput value ->
            let newModel = { model.BookModel with TitleInput = value }
            { model with BookModel = newModel }, Cmd.none
        | SetAuthorId value ->
            let newModel = { model.BookModel with AuthorId = value }
            { model with BookModel = newModel }, Cmd.none
        | GetAuthor ->
            let cmd = Cmd.OfAsync.perform bookstoreApi.getAuthor (Guid.Parse(model.BookModel.AuthorId)) AddBook
            let newModel = { model.BookModel with AuthorId = "" }
            { model with BookModel = newModel }, Cmd.map BookMsg cmd 
        | AddBook author ->
            let book = Book.create (model.BookModel.TitleInput, author)
            let cmd = Cmd.OfAsync.perform bookstoreApi.addBook book AddedBook
            let newModel = { model.BookModel with TitleInput = "" }
            { model with BookModel = newModel }, Cmd.map BookMsg cmd 
        | AddedBook book ->
            let newModel = { model.BookModel with Books = model.BookModel.Books @ [ book ] }
            { model with BookModel = newModel }, Cmd.none
    | AuthorMsg authorMsg ->
        match authorMsg with
        | SetFirstNameInput value ->
            let newModel = { model.AuthorModel with FirstNameInput = value }
            { model with AuthorModel = newModel }, Cmd.none
        | SetLastNameInput value ->
            let newModel = { model.AuthorModel with LastNameInput = value }
            { model with AuthorModel = newModel }, Cmd.none
        | AddAuthor ->
            let author = Author.create (model.AuthorModel.FirstNameInput, model.AuthorModel.LastNameInput)
            let cmd = Cmd.OfAsync.perform bookstoreApi.addAuthor author AddedAuthor
            let newModel = { model.AuthorModel with FirstNameInput = ""; LastNameInput = "" }
            { model with AuthorModel = newModel }, Cmd.map AuthorMsg cmd 
        | AddedAuthor author ->
            let newModel = { model.AuthorModel with Authors = model.AuthorModel.Authors @ [ author ] }
            { model with AuthorModel = newModel }, Cmd.ofMsg (SwitchPage Page.BookList)
    | SwitchPage page ->
        let updatedModel =
            match page with
            | Page.BookList ->
                let newModel = { model.BookModel with Authors = model.AuthorModel.Authors }
                { model with BookModel = newModel }
            | Page.CreateAuthor ->
                let newModel = { model.AuthorModel with Authors = model.BookModel.Authors }
                { model with AuthorModel = newModel }
        { updatedModel with CurrentPage = page}, Cmd.none
    

open Feliz
open Feliz.Bulma

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

let authorDropdownOptions (authors: Author list) =
    authors
    |> List.map (fun author ->
        Html.option [
            prop.value (author.Id.ToString())
            prop.text (sprintf "%s %s" author.FirstName author.LastName)
        ])

let booklistContainerBox (model: BookModel) (dispatch: BookMsg -> unit) =
    model.Authors |> List.iter (fun a -> printf "!!!!!!!!!!!!!!!!!!!!!!!%s" a.FirstName)
    Bulma.box [
        Bulma.content [
            Html.ol [
                for book in model.Books do
                    Html.li [ prop.text book.Title ]
            ]
        ]
        Bulma.field.div [
            field.isGrouped
            prop.children [
                Bulma.control.p [
                    control.isExpanded
                    prop.children [
                        Bulma.input.text [
                            prop.value model.TitleInput
                            prop.placeholder "Specify book title"
                            prop.onChange (fun x -> SetTitleInput x |> dispatch)
                        ]
                        Bulma.select [
                            prop.onChange (fun id -> SetAuthorId id |> dispatch)
                            prop.children (authorDropdownOptions model.Authors)
                        ]
                    ]
                ]
                Bulma.control.p [
                    Bulma.button.a [
                        color.isPrimary
                        prop.disabled (String.IsNullOrEmpty model.TitleInput || String.IsNullOrEmpty model.AuthorId)
                        prop.onClick (fun _ -> dispatch GetAuthor)
                        prop.text "Add"
                    ]
                ]
            ]
        ]
    ]

let authorContainerBox (model:AuthorModel) (dispatch: AuthorMsg -> unit) =
    Bulma.box [
        Bulma.content [
            Html.ol [
                for author in model.Authors do
                    Html.li [ prop.text (sprintf "%s %s" author.FirstName author.LastName) ]
            ]
        ]
        Bulma.field.div [
            field.isGrouped
            prop.children [
                Bulma.control.p [
                    control.isExpanded
                    prop.children [
                        Bulma.input.text [
                            prop.value model.FirstNameInput
                            prop.placeholder "Author's first name"
                            prop.onChange (fun x -> SetFirstNameInput x |> dispatch)
                        ]
                        Bulma.input.text [
                            prop.value model.LastNameInput
                            prop.placeholder "Author's last name"
                            prop.onChange (fun x -> SetLastNameInput x |> dispatch)
                        ]
                    ]
                ]
                Bulma.control.p [
                    Bulma.button.a [
                        color.isPrimary
                        prop.disabled (Author.isValid(model.FirstNameInput, model.LastNameInput) |> not)
                        prop.onClick (fun _ -> dispatch AddAuthor)
                        prop.text "Add"
                    ]
                ]
            ]
        ]
    ]

let view (model:Model) (dispatch: Msg -> unit) =
    let booklistDispatch (bookMsg: BookMsg) : unit =
        dispatch (Msg.BookMsg bookMsg)

    let authorDispatch (authorMsg: AuthorMsg) : unit =
        dispatch (Msg.AuthorMsg authorMsg)

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
                            | Page.BookList -> booklistContainerBox model.BookModel booklistDispatch
                            | Page.CreateAuthor -> authorContainerBox model.AuthorModel authorDispatch
                        ]
                    ]
                ]
            ]
        ]
    ]