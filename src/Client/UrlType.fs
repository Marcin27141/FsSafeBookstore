module UrlType

open Feliz.Router
open Shared
open System

[<RequireQualifiedAccess>]
type BooklistUrl =
  | Booklist
  | CreateAuthor
  | BookDetails of Guid

[<RequireQualifiedAccess>]
type Url =
  | AfterLogin
  | BooklistUrl of BooklistUrl
  | NotFound

let parseUrl = function
    | [ ] -> Url.AfterLogin
    | [ "booklist"; "books" ] -> Url.BooklistUrl BooklistUrl.Booklist
    | [ "booklist"; "authors" ] -> Url.BooklistUrl BooklistUrl.CreateAuthor
    | [ "booklist"; "details"; Route.Guid bookId ] -> Url.BooklistUrl (BooklistUrl.BookDetails bookId)
    | _ -> Url.NotFound