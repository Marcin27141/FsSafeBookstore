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
| BookDetails of Guid
| AuthorDetails of Guid
| NotFound

let getBooklistPrefix =
    [|"booklist"|]

let appendToBooklist suffix =
    Array.append getBooklistPrefix suffix

let getUrlForPage (page: Page) =
    match page with
    | Login -> [|"login"|]
    | Home -> [|""|]
    | Booklist -> appendToBooklist [|"books"|]
    | CreateAuthor -> appendToBooklist [|"create"; "authors"|]
    | CreateBook -> appendToBooklist [|"create"; "books "|]
    | BookDetails id -> appendToBooklist [|"books"; id.ToString() |]
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
