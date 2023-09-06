[<RequireQualifiedAccess>]
module BooksIndex

open ViewUtils
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
    | CreateBook of CreateBook.Model
    | EditBook of EditBook.Model
    | NotFound

[<RequireQualifiedAccess>]
type Url =
  | Booklist
  | CreateBook
  | EditBook of Guid
  | BookDetails of Guid
  | NotFound

let parseUrl = function
    | [] -> Url.Booklist
    | [ "create" ] -> Url.CreateBook
    | [ "edit"; Route.Guid bookId ] -> Url.EditBook bookId
    | [ Route.Guid bookId ] -> Url.BookDetails bookId
    | _ -> Url.NotFound

type Model = { CurrentPage: Page; CurrentUrl: Url }

type Msg =
    | BooklistMsg of Booklist.Msg
    | CreateBookMsg of CreateBook.Msg
    | EditBookMsg of EditBook.Msg
    | BookDetailsMsg of BookDetails.Msg
    | SwitchToBookDetails of Book
    | GotBookForDetails of Option<Book>
    | GotBookForEdit of Option<Book>

let bookstoreApi = getApiProxy ()

let loadBookFromApi model id msg =
    let cmd = async {
        let! book = bookstoreApi.getBook id
        return (msg book)
    }
    { model with CurrentPage = Page.Loading }, Cmd.OfAsync.result cmd
    
let init (currentUrl: Url) : Model * Cmd<Msg> =
    let modelWithUrl = { CurrentUrl = currentUrl; CurrentPage = Page.NotFound }
    match currentUrl with
    | Url.Booklist ->
        let model, cmd = Booklist.init ()
        { modelWithUrl with CurrentPage = Page.BookList model}, Cmd.map BooklistMsg cmd
    | Url.CreateBook ->
        let model, cmd = CreateBook.init ()
        { modelWithUrl with CurrentPage = Page.CreateBook model}, Cmd.map CreateBookMsg cmd
    | Url.EditBook id ->
        loadBookFromApi modelWithUrl id Msg.GotBookForEdit
    | Url.BookDetails id ->
        loadBookFromApi modelWithUrl id Msg.GotBookForDetails
    | Url.NotFound ->
        modelWithUrl, Cmd.none

let updateBooklistModel msg booklistModel model =
    let updatedBooklistModel, cmd, intent = Booklist.update msg booklistModel
    let updatedModel = { model with CurrentPage = Page.BookList updatedBooklistModel }
    match intent with
    | Booklist.Intent.ShowBookDetails book ->
        updatedModel, Cmd.ofMsg (SwitchToBookDetails book)
    | Booklist.Intent.DoNothing ->
        updatedModel, Cmd.map BooklistMsg cmd

let updateCreateBookModel msg createBookModel model =
    let updatedCreateBookModel, cmd, intent = CreateBook.update msg createBookModel
    let updatedModel = { model with CurrentPage = Page.CreateBook updatedCreateBookModel }
    match intent with
    | CreateBook.Intent.ShowAddedBook book ->
        updatedModel, Cmd.ofMsg (SwitchToBookDetails book)
    | _ ->
        updatedModel, Cmd.map CreateBookMsg cmd

let updateEditBookModel msg editBookModel model =
    let updatedEditBookModel, cmd, intent = EditBook.update msg editBookModel
    let updatedModel = { model with CurrentPage = Page.EditBook updatedEditBookModel }
    match intent with
    | EditBook.Intent.ShowEditedBook book ->
        updatedModel, Cmd.ofMsg (SwitchToBookDetails book)
    | EditBook.Intent.DoNothing ->
        updatedModel, Cmd.map EditBookMsg cmd

let updateBookDetailskModel msg bookDetailsModel model =
    let updatedDetailsModel, cmd, intent = BookDetails.update msg bookDetailsModel
    let updatedModel = { model with CurrentPage = Page.BookDetails updatedDetailsModel }
    match intent with
    | BookDetails.Intent.EditBook book ->
        model, Cmd.navigate("booklist", "books", "edit", book.Id.ToString() )
    | BookDetails.Intent.DoNothing ->
        updatedModel, Cmd.map BookDetailsMsg cmd

let update (msg: Msg) (model:Model) :Model * Cmd<Msg> =
    match model.CurrentPage, msg with
    | _, GotBookForEdit book ->
        match book with
        | Some bookValue ->
            let editModel, cmd = EditBook.init bookValue
            { model with CurrentPage = Page.EditBook editModel}, Cmd.map EditBookMsg cmd
        | None ->
            { model with CurrentPage = Page.NotFound }, Cmd.none
    | _, GotBookForDetails book ->
        match book with
        | Some bookValue ->
            let detailsModel, cmd = BookDetails.init bookValue
            { model with CurrentPage = Page.BookDetails detailsModel}, Cmd.map BookDetailsMsg cmd
        | None ->
            { model with CurrentPage = Page.NotFound }, Cmd.none
    | Page.BookList bookModel, BooklistMsg bookMsg ->
        updateBooklistModel bookMsg bookModel model
    | Page.CreateBook bookModel, CreateBookMsg bookMsg ->
        updateCreateBookModel bookMsg bookModel model
    | Page.BookDetails detailsModel, BookDetailsMsg detailsMsg ->
        updateBookDetailskModel detailsMsg detailsModel model
    | Page.EditBook editModel, EditBookMsg editMsg ->
        updateEditBookModel editMsg editModel model
    | _, SwitchToBookDetails book ->
        model, Cmd.navigate("booklist", "books", book.Id.ToString() )
    | _, _ -> model, Cmd.none

let render (model:Model) (dispatch: Msg -> unit) =
    match model.CurrentPage with
    | Page.Loading -> Html.div [ Bulma.pageLoader.isActive ]
    | Page.BookList model -> Booklist.render model (Msg.BooklistMsg >> dispatch)
    | Page.CreateBook model -> CreateBook.render model (Msg.CreateBookMsg >> dispatch)
    | Page.EditBook model -> EditBook.render model (Msg.EditBookMsg >> dispatch)
    | Page.BookDetails model -> BookDetails.render model (Msg.BookDetailsMsg >> dispatch)
    | Page.NotFound -> getPageNotFoundContent ()