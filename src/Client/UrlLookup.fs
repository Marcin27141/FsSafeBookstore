module UrlLookup

open System
open Feliz.Router

[<RequireQualifiedAccess>]
type IndexUrl =
  | BooklistUrl
  | CreateAuthorUrl
  | CreateBookUrl
  | BookDetailsUrl
  | AuthorDetailsUrl

[<RequireQualifiedAccess>]
type HomeUrl =
  | Home
  | IndexUrl of IndexUrl
  | NotFoundUrl

[<RequireQualifiedAccess>]
type Url =
  | AfterLoginUrl of HomeUrl
  | LoginUrl
  | NotFoundUrl

type Page =
| Login
| Home
| Booklist
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

let getUrlForPage (page: Page) =
    match page with
    | Login -> [|"login"|]
    | Home -> [|""|]
    | Booklist -> appendToBooksIndex [||]
    | CreateAuthor -> appendToBooklist [|"create"; "authors"|]
    | CreateBook -> appendToBooksIndex [|"create"|]
    | EditBook id-> appendToBooksIndex [|"edit"; id.ToString()|]
    | BookDetails id -> appendToBooksIndex [| id.ToString() |]
    | AuthorDetails id -> appendToBooklist [| "authors"; id.ToString() |]
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