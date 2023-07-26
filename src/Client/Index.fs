module Index

open Elmish
open Fable.Remoting.Client
open Shared

type Model = { Books: Book list; TitleInput: string; FirstNameInput: string; LastNameInput: string }

type Msg =
    | GotBooks of Book list
    | SetTitleInput of string
    | SetFirstNameInput of string
    | SetLastNameInput of string
    | AddBook
    | AddedBook of Book

let bookstoreApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IBookstoreApi>

let init () : Model * Cmd<Msg> =
    let model = { Books = []; TitleInput = ""; FirstNameInput = ""; LastNameInput = ""}

    let cmd = Cmd.OfAsync.perform bookstoreApi.getBooks () GotBooks

    model, cmd

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | GotBooks books -> { model with Books = books }, Cmd.none
    | SetTitleInput value -> { model with TitleInput = value }, Cmd.none
    | SetFirstNameInput value -> { model with FirstNameInput = value }, Cmd.none
    | SetLastNameInput value -> { model with LastNameInput = value }, Cmd.none
    | AddBook ->
        let author = Author.create (model.FirstNameInput, model.LastNameInput)
        let book = Book.create (model.TitleInput, author)

        let cmd = Cmd.OfAsync.perform bookstoreApi.addBook book AddedBook

        { model with TitleInput = ""; FirstNameInput = ""; LastNameInput = "" }, cmd
    | AddedBook book -> { model with Books = model.Books @ [ book ] }, Cmd.none

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

let containerBox (model: Model) (dispatch: Msg -> unit) =
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
                        Bulma.input.text [
                            prop.value model.FirstNameInput
                            prop.placeholder "Author first name"
                            prop.onChange (fun x -> SetFirstNameInput x |> dispatch)
                        ]
                        Bulma.input.text [
                            prop.value model.LastNameInput
                            prop.placeholder "Author last name"
                            prop.onChange (fun x -> SetLastNameInput x |> dispatch)
                        ]
                    ]
                ]
                Bulma.control.p [
                    Bulma.button.a [
                        color.isPrimary
                        prop.disabled (Book.isValid (model.TitleInput, Author.create(model.FirstNameInput, model.LastNameInput)) |> not)
                        prop.onClick (fun _ -> dispatch AddBook)
                        prop.text "Add"
                    ]
                ]
            ]
        ]
    ]

let view (model: Model) (dispatch: Msg -> unit) =
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
                            containerBox model dispatch
                        ]
                    ]
                ]
            ]
        ]
    ]