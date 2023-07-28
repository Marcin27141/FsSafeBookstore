module Server.Tests

open Expecto

open Shared
open Server

let server = testList "Server" [
    testCase "Adding valid Author" <| fun _ ->
        let validAuthor = Author.create ("William", "Shakespeare")
        assert Author.isValid (validAuthor.FirstName, validAuthor.LastName)
        let expectedResult = Ok ()

        let result = Storage.addAuthor validAuthor

        Expect.equal result expectedResult "Result should be ok"
        Expect.contains Storage.authors validAuthor "Storage should contain new author"
]

let all =
    testList "All"
        [
            Shared.Tests.shared
            server
        ]

[<EntryPoint>]
let main _ = runTestsWithCLIArgs [] [||] all