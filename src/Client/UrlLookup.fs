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
    | Home -> [|"home"|]
    | Booklist -> appendToBooksIndex [||]
    | Authorslist -> appendToAuthorsIndex [||]
    | CreateAuthor -> appendToAuthorsIndex [|"create"|]
    | CreateBook -> appendToBooksIndex [|"create"|]
    | EditBook id-> appendToBooksIndex [|"edit"; id.ToString()|]
    | BookDetails id -> appendToBooksIndex [| id.ToString() |]
    | AuthorDetails id -> appendToAuthorsIndex [| id.ToString() |]
    | NotFound -> [|"404"|]