module Server

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Saturn

open Shared

module Storage =
    let books = ResizeArray()

    let addBook (book: Book) =
        if Book.isValid (book.Title, book.Author) then
            books.Add book
            Ok()
        else
            Error "Invalid book"

    do
        addBook (Book.create ("Hamlet", Author.create ("William", "Shakespeare"))) |> ignore
        addBook (Book.create ("Pan Tadeusz", Author.create ("Adam", "Mickiewicz"))) |> ignore
        addBook (Book.create ("Kordian", Author.create ("Juliusz", "Słowacki"))) |> ignore

let bookstoreApi =
    { getBooks = fun () -> async { return Storage.books |> List.ofSeq }
      addBook =
        fun book ->
            async {
                return
                    match Storage.addBook book with
                    | Ok () -> book
                    | Error e -> failwith e
            } }

let webApp =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromValue bookstoreApi
    |> Remoting.buildHttpHandler

let app =
    application {
        use_router webApp
        memory_cache
        use_static "public"
        use_gzip
    }

[<EntryPoint>]
let main _ =
    run app
    0

//module Storage =
//    let todos = ResizeArray()

//    let addTodo (todo: Todo) =
//        if Todo.isValid todo.Description then
//            todos.Add todo
//            Ok()
//        else
//            Error "Invalid todo"

//    do
//        addTodo (Todo.create "Create new SAFE project")
//        |> ignore

//        addTodo (Todo.create "Write your app") |> ignore
//        addTodo (Todo.create "Ship it !!!") |> ignore

//let todosApi =
//    { getTodos = fun () -> async { return Storage.todos |> List.ofSeq }
//      addTodo =
//        fun todo ->
//            async {
//                return
//                    match Storage.addTodo todo with
//                    | Ok () -> todo
//                    | Error e -> failwith e
//            } }

//let webApp =
//    Remoting.createApi ()
//    |> Remoting.withRouteBuilder Route.builder
//    |> Remoting.fromValue todosApi
//    |> Remoting.buildHttpHandler

//let app =
//    application {
//        use_router webApp
//        memory_cache
//        use_static "public"
//        use_gzip
//    }

//[<EntryPoint>]
//let main _ =
//    run app
//    0