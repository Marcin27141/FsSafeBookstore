namespace Shared

open System

[<CLIMutable>]
type Author = {Id: Guid; FirstName: string; LastName: string}

[<CLIMutable>]
type Book = { Id: Guid; Title: string; Author: Author }

[<CLIMutable>]
type User = { Id: Guid; Username: string; Password: string; FirstName: string; LastName: string }

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

module User =
    let isValid (username : string, password: string, firstName: string, lastName: string) =
        match username, password, firstName, lastName with
        | "", _, _, _ -> Error "Username can't be empty"
        | _, "", _, _ -> Error "Password can't be empty"
        | _, _, fn, _ when String.IsNullOrWhiteSpace fn || not (fn |> Seq.forall System.Char.IsLetter) ->
            Error "First name can't be empty and must contain only letters"
        | _, _, _, ln when String.IsNullOrWhiteSpace ln || not (ln |> Seq.forall System.Char.IsLetter) ->
            Error "Last name can't be empty and must contain only letters"
        | _ -> Ok ()

    let create (username : string, password: string, firstName: string, lastName: string) =
        { Id = Guid.NewGuid()
          Username = username
          Password = password
          FirstName = firstName
          LastName = lastName }

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

type IAuthorsApi =
    { getAuthors: unit -> Async<Author list>
      addAuthor: Author -> Async<Author>
      deleteAuthor: Author -> Async<bool>
      editAuthor: Author * Author -> Async<bool> }

type IBooksApi =
    { getBooks: unit -> Async<Book list>
      addBook: Book -> Async<Book>
      deleteBook: Book -> Async<bool>
      editBook: Book * Book -> Async<bool> }

type IBookstoreIndexApi =
    {
        getAuthor: Guid -> Async<Option<Author>>
        getBook: Guid -> Async<Option<Book>>
    }

type RegisterRequest = {
    Username: string
    Password: string
    FirstName: string
    LastName: string
}

type LoginRequest = {
    Username: string
    Password: string
}

type RegistrationResult =
    | Registered of User
    | RegistrationError of string

type LoginResult =
    | UsernameOrPasswordIncorrect
    | LoggedIn of User

type RegistrationProcess =
    | NotStarted
    | InProgress
    | Finished of RegistrationResult

type LoginProcess =
    | NotStarted
    | InProgress
    | Finished of LoginResult

type IUserApi =
    {
        register: RegisterRequest -> Async<RegistrationResult>
        login: LoginRequest -> Async<LoginResult>
    }

type IServerApi =
    {
      getBooks: unit -> Async<Book list>
      getBook: Guid -> Async<Option<Book>>
      addBook: Book -> Async<Book>
      editBook: Book * Book -> Async<bool>
      deleteBook: Book -> Async<bool>
      getAuthor: Guid -> Async<Option<Author>>
      getAuthors: unit -> Async<Author list>
      addAuthor: Author -> Async<Author>
      editAuthor: Author * Author -> Async<bool>
      deleteAuthor: Author -> Async<bool>
      register: RegisterRequest -> Async<RegistrationResult>
      login: LoginRequest -> Async<LoginResult>
    }