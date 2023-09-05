module Server

open Saturn
open Shared
open System
open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open LiteDB.FSharp
open LiteDB

module Storage =
    let database =
        let mapper = FSharpBsonMapper()
        let connStr = "Filename=Bookstore.db;mode=Exclusive"
        new LiteDatabase (connStr, mapper)
    let authors = database.GetCollection<Author> "authors"

    let getAuthors () =
        authors.FindAll () |> List.ofSeq

    let addAuthor (author: Author) =
        if Author.isValid (author.FirstName, author.LastName) then
            authors.Insert author |> ignore
            true
        else
            false

    let deleteAuthor (author: Author) =
        authors.Delete author.Id

    let editAuthor (oldAuthor: Author, editedAuthor: Author) =
        if oldAuthor.Id = editedAuthor.Id then
            match authors.Delete oldAuthor.Id with
            | true ->
                addAuthor editedAuthor
            | _ -> false
        else false

    do
        if getAuthors () |> Seq.isEmpty then
            addAuthor (Author.create ("William", "Shakespeare")) |> ignore
            addAuthor (Author.create ("Adam", "Mickiewicz")) |> ignore
            addAuthor (Author.create ("Juliusz", "Słowacki")) |> ignore

    let books = database.GetCollection<Book> "books"

    let getBooks () =
        books.FindAll () |> List.ofSeq

    let addBook (book: Book) =
        if Book.isValid (book.Title, book.Author) then
            books.Insert book|> ignore
            true
        else
            false

    let deleteBook (book: Book) =
        books.Delete book.Id

    let editBook (oldBook: Book, editedBook: Book) =
        if oldBook.Id = editedBook.Id then
            match books.Delete oldBook.Id with
            | true ->
                addBook editedBook
            | _ -> false
        else false

let authorsApi : IAuthorsApi =
    {
        getAuthors = fun () -> async { return Storage.getAuthors () }
        addAuthor =
            fun author ->
                async {
                    return
                        match Storage.addAuthor author with
                        | true -> author
                        | false -> failwith "Error"
                }
        deleteAuthor = fun author -> async { return Storage.deleteAuthor author }
        editAuthor =
            fun (oldAuthor, editedAuthor) ->
                async {
                    return Storage.editAuthor (oldAuthor, editedAuthor)
                }
    }

let bookstoreIndexApi =
    {
        getAuthor = fun id -> async { return Storage.getAuthors () |> List.find (fun (author: Author) -> author.Id = id) }
        getBook = fun id -> async { return Storage.getBooks () |> List.find (fun (book: Book) -> book.Id = id) }
    }

let booksApi : IBooksApi =
    {
        getBooks = fun () -> async { return Storage.getBooks () }
        addBook =
            fun book ->
                async {
                    return
                        match Storage.addBook book with
                        | true -> book
                        | false -> failwith "Error"
                }
        deleteBook = fun book -> async { return Storage.deleteBook book }
        editBook =
            fun (oldBook, editedBook) ->
                async {
                    return Storage.editBook (oldBook, editedBook)
                }
    }

let userApi =
    { login =
        fun loginRequest ->
            let user, password = loginRequest.Username, loginRequest.Password
            async {
                do! Async.Sleep 1500
                match user, password with
                | x, y when x = y ->
                    let accessToken = Guid.NewGuid().ToString()
                    return LoggedIn { Username = user; AccessToken = AccessToken accessToken }
                | _, _ -> return UsernameOrPasswordIncorrect
            }
    }

let serverApi =
    {
        getBooks = booksApi.getBooks
        getBook = bookstoreIndexApi.getBook
        addBook = booksApi.addBook
        editBook = booksApi.editBook
        deleteBook = booksApi.deleteBook
        getAuthor = bookstoreIndexApi.getAuthor
        deleteAuthor = authorsApi.deleteAuthor
        getAuthors = authorsApi.getAuthors
        addAuthor = authorsApi.addAuthor
        editAuthor = authorsApi.editAuthor
        login = userApi.login
    }

let bookstoreApp =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Shared.Route.builder
    |> Remoting.fromValue serverApi
    |> Remoting.buildHttpHandler

let app =
    application {
        use_router bookstoreApp
        memory_cache
        use_static "public"
        use_gzip
    }

[<EntryPoint>]
let main _ =
    run app
    0