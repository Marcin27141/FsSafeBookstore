module Server

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Saturn

open Shared
open System
open Microsoft
open Giraffe
open Microsoft.AspNetCore.Http

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

let bookstoreApi =
    { getBooks = fun () -> async { return Storage.books |> List.ofSeq }
      addBook =
        fun book ->
            async {
                return
                    match Storage.addBook book with
                    | Ok () -> book
                    | Error e -> failwith e
            }
      getAuthor = fun id -> async { return Storage.authors |> Seq.find (fun (author: Author) -> author.Id = id) }
      getAuthors = fun () -> async { return Storage.authors |> List.ofSeq }
      addAuthor =
        fun author ->
            async {
                return
                    match Storage.addAuthor author with
                    | Ok () -> author
                    | Error e -> failwith e
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

type apiType = Giraffe.Core.HttpFunc -> AspNetCore.Http.HttpContext -> Giraffe.Core.HttpFuncResult

//let bookstoreApp : Giraffe.Core.HttpFunc -> AspNetCore.Http.HttpContext -> Giraffe.Core.HttpFuncResult =
//    Remoting.createApi ()
//    |> Remoting.withRouteBuilder Route.builder
//    |> Remoting.fromValue bookstoreApi
//    |> Remoting.buildHttpHandler

//let bookstoreApp : HttpHandler =
//    choose [
//        GET "/api/books" >=> fun _ ->
//            async {
//                let! books = bookstoreApi.getBooks ()
//                return (Ok books)
//            }
//        POST "/api/books" >=> fun ctx ->
//            let book = ctx.TryExtractJson<Book>()
//            async {
//                let! addedBook = bookstoreApi.addBook book
//                match addedBook book with
//                | Ok () -> return OK book
//                | Error e -> return BadRequest e
//            }
//        GET "/api/authors/{id}" >=> fun (ctx: HttpContext) ->
//            let id = ctx.RouteParams.["id"]
//            async {
//                try
//                    let! author = bookstoreApi.getAuthor (Guid.Parse(id))
//                    match author book with
//                    | Author _ -> return OK author
//                    | Error e -> return NotFound
//                with
//                | :? System.Collections.Generic.KeyNotFoundException ->
//                    return NotFound
//            }

//        GET "/api/authors" >=> fun _ ->
//            async {
//                let! authors = bookstoreApi.getAuthors ()
//                return Ok authors
//            }
//        POST "/api/authors" >=> fun ctx ->
//            let author = ctx.TryExtractJson<Book>()
//            async {
//                let! addedBook = bookstoreApi.addBook book
//                match addedBook book with
//                | Ok () -> return OK book
//                | Error e -> return BadRequest e
//            }
//    ]

let userApp =
    choose [
        POST >=> fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let! loginRequest = ctx.BindJsonAsync<LoginRequest>()
                let! result = Async.StartAsTask (userApi.login loginRequest)
                return! Successful.OK result next ctx
            }
    ]

let bookstoreApp : apiType =
    choose [
        GET >=> fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let! result = Async.StartAsTask (bookstoreApi.getAuthors ())
                return! Successful.OK result next ctx
            }
            //json (Async.RunSynchronously (bookstoreApi.getAuthors (), 10000))
            //json ("Not working")
        ]

let testApp : apiType =
    choose [
        GET >=>
            json ("It is a test")
        ]

//let userApp : Giraffe.Core.HttpFunc -> AspNetCore.Http.HttpContext -> Giraffe.Core.HttpFuncResult =
//    Remoting.createApi ()
//    |> Remoting.withRouteBuilder Route.builder
//    |> Remoting.fromValue userApi
//    |> Remoting.buildHttpHandler



let apiRouter : apiType =
    choose [
        route "/api/login"    >=> userApp
        route "/api/authors"    >=> bookstoreApp
        route "/api/test" >=> testApp
    ]

let app =
    application {
        use_router apiRouter
        memory_cache
        use_static "public"
        use_gzip
    }

[<EntryPoint>]
let main _ =
    run app
    0