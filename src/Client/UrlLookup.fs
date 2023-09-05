module UrlLookup

open System
open Feliz.Router

type Page =
| Login
| Home
| Booklist
| Authorslist
| CreateAuthor
| CreateBook
| EditBook of Guid
| BookDetails of Guid
| AuthorDetails of Guid
| NotFound

let appendToBooklist suffix =
    let getBooklistPrefix =
        [|"booklist"|]
    Array.append getBooklistPrefix suffix

let appendToBooksIndex suffix =
    let getBooksIndexPrefix =
        appendToBooklist [|"books"|]
    Array.append getBooksIndexPrefix suffix

let appendToAuthorsIndex suffix =
    let getAuthorsIndexPrefix =
        appendToBooklist [|"authors"|]
    Array.append getAuthorsIndexPrefix suffix

let getUrlForPage (page: Page) =
    match page with
    | Login -> [|"login"|]
    | Home -> [|""|]
    | Booklist -> appendToBooksIndex [||]
    | Authorslist -> appendToAuthorsIndex [||]
    | CreateAuthor -> appendToAuthorsIndex [|"create"|]
    | CreateBook -> appendToBooksIndex [|"create"|]
    | EditBook id-> appendToBooksIndex [|"edit"; id.ToString()|]
    | BookDetails id -> appendToBooksIndex [| id.ToString() |]
    | AuthorDetails id -> appendToAuthorsIndex [| id.ToString() |]
    | NotFound -> [|"404"|]

//let getPageForUrl url =
//    let getBooklistPrefixList ()= List.ofArray getBooklistPrefix
//    let appendBooklistPrefix suffix = (List.ofArray getBooklistPrefix) @ suffix
//    let appendBooklistPrefixWithGuid suffix = (List.ofArray getBooklistPrefix) @ suffix
//    match url with
//    | ["login"] -> Login
//    | [""] -> Home
//    | _ when url = appendBooklistPrefix ["books"] -> Booklist
//    | _ when url = appendBooklistPrefix ["create"; "authors"] -> CreateAuthor
//    | _ when url = appendBooklistPrefix ["create"; "books "] -> CreateBook
//    | prefix :: ["books" :: id] when prefix = getBooklistPrefixList () &&  appendBooklistPrefix ["books"; id ] -> BookDetails bookId
//    | _ when url = appendBooklistPrefix [ "authors"; Route.Guid authorId ] -> AuthorDetails authorId
//    | _ -> NotFound
