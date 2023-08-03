module Server

open Saturn
open Shared
open System
open Fable.Remoting.Server
open Fable.Remoting.Giraffe

module Storage =
    let authors = ResizeArray()
    let books = ResizeArray()

    let addAuthor (author: Author) =
        if Author.isValid (author.FirstName, author.LastName) then
            authors.Add author
            Ok()
        else
            Error "Invalid author"
    do
        addAuthor (Author.create ("William", "Shakespeare")) |> ignore
        addAuthor (Author.create ("Adam", "Mickiewicz")) |> ignore
        addAuthor (Author.create ("Juliusz", "Słowacki")) |> ignore

    let addBook (book: Book) =
        if Book.isValid (book.Title, book.Author) then
            books.Add book
            Ok()
        else
            Error "Invalid book"

    //do
    //    addBook (Book.create ("Hamlet", Author.create ("William", "Shakespeare"))) |> ignore
    //    addBook (Book.create ("Pan Tadeusz", Author.create ("Adam", "Mickiewicz"))) |> ignore
    //    addBook (Book.create ("Kordian", Author.create ("Juliusz", "Słowacki"))) |> ignore

let authorsApi : IAuthorsApi =
    { getAuthors = fun () -> async { return Storage.authors |> List.ofSeq }
      addAuthor =
        fun author ->
            async {
                return
                    match Storage.addAuthor author with
                    | Ok () -> author
                    | Error e -> failwith e
            }
            }

let booksApi =
    { getBooks = fun () -> async { return Storage.books |> List.ofSeq }
      addBook =
        fun book ->
            async {
                return
                    match Storage.addBook book with
                    | Ok () -> book
                    | Error e -> failwith e
            }
     }


let bookstoreIndexApi =
    {
        getAuthor = fun id -> async { return Storage.authors |> Seq.find (fun (author: Author) -> author.Id = id) }
        getBook = fun id -> async { return Storage.books |> Seq.find (fun (book: Book) -> book.Id = id) }
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
        getAuthor = bookstoreIndexApi.getAuthor
        getAuthors = authorsApi.getAuthors
        addAuthor = authorsApi.addAuthor
        login = userApi.login
    }

//type apiType = Giraffe.Core.HttpFunc -> AspNetCore.Http.HttpContext -> Giraffe.Core.HttpFuncResult

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


//let apiRouter : apiType =
//    choose [
//        route "/api/login"    >=> userApp
//        route "/api/authors"    >=> bookstoreApp
//        route "/api/test" >=> testApp
//    ]