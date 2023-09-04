namespace Shared

open System

type Author = {Id: Guid; FirstName: string; LastName: string}
type Book = { Id: Guid; Title: string; Author: Author }

module Author =
    let isValid (firstName : string, lastName: string) =
        not (String.IsNullOrWhiteSpace firstName)
        && not (String.IsNullOrWhiteSpace lastName)

    let create (firstName : string, lastName: string) =
        { Id = Guid.NewGuid()
          FirstName = firstName
          LastName = lastName }

module Book =
    let isValid (title: string, author: Author) =
        not (String.IsNullOrWhiteSpace title)
        && Author.isValid(author.FirstName, author.LastName)

    let create (title: string, author: Author) =
        { Id = Guid.NewGuid()
          Title = title
          Author = author }

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

type IAuthorsApi =
    { getAuthors: unit -> Async<Author list>
      addAuthor: Author -> Async<Author>
      deleteAuthor: Author -> Async<bool> }

type IBooksApi =
    { getBooks: unit -> Async<Book list>
      addBook: Book -> Async<Book>
      deleteBook: Book -> Async<bool> }

type IBookstoreIndexApi =
    {
        getAuthor: Guid -> Async<Author>
        getBook: Guid -> Async<Book>
    }

type AccessToken = AccessToken of string

type User =
    { Username : string
      AccessToken : AccessToken }

type LoginRequest = {
    Username: string
    Password: string
}

type LoginResult =
    | UsernameOrPasswordIncorrect
    | LoggedIn of User

type LoginProcess =
    | NotStarted
    | InProgress
    | Finished of LoginResult

type IUserApi =
    { login: LoginRequest -> Async<LoginResult> }

type IServerApi =
    {
      getBooks: unit -> Async<Book list>
      getBook: Guid -> Async<Book>
      addBook: Book -> Async<Book>
      deleteBook: Book -> Async<bool>
      getAuthor: Guid -> Async<Author>
      getAuthors: unit -> Async<Author list>
      addAuthor: Author -> Async<Author>
      deleteAuthor: Author -> Async<bool>
      login: LoginRequest -> Async<LoginResult>
    }