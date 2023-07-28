module Shared.Tests

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

open Shared

let shared = testList "Shared" [
    testCase "Empty string is not a valid title" <| fun _ ->
        let expected = false
        let validAuthor = Author.create ("Valid", "Valid")
        assert Author.isValid (validAuthor.FirstName, validAuthor.LastName)
        let actual = Book.isValid ("", validAuthor)
        Expect.equal actual expected "Empty string is not valid title"

    testCase "Empty string is not a valid first name" <| fun _ ->
        let expected = false
        let actual = Author.isValid ("", "Shakespeare")
        Expect.equal actual expected "Should be false"

    testCase "Empty string is not a valid last name" <| fun _ ->
        let expected = false
        let actual = Author.isValid ("William", "")
        Expect.equal actual expected "Should be false"
]