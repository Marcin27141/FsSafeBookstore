[<RequireQualifiedAccess>]
module AuthorsIndex

open Elmish
open Fable.Remoting.Client
open Shared
open System
open Feliz
open Feliz.Bulma
open Feliz.Router
open ApiProxy

[<RequireQualifiedAccess>]
type Page =
    | Loading
    | AuthorDetails of AuthorDetails.Model
    | Authorslist of Authorslist.Model
    | CreateAuthor of CreateAuthor.Model
    | NotFound

[<RequireQualifiedAccess>]
type Url =
  | CreateAuthor
  | Authorslist
  | AuthorDetails of Guid
  | NotFound

let parseUrl = function
    | [] -> Url.Authorslist
    | [ "create" ] -> Url.CreateAuthor
    | [ Route.Guid authorId ] -> Url.AuthorDetails authorId
    | _ -> Url.NotFound

type Model = { CurrentPage: Page; CurrentUrl: Url }

type Msg =
    | CreateAuthorMsg of CreateAuthor.Msg
    | AuthorslistMsg of Authorslist.Msg
    | AuthorDetailsMsg of AuthorDetails.Msg
    | GotAuthorForDetails of Author
    | SwitchToAuthorDetails of Author

let bookstoreApi = getApiProxy ()

let loadAuthorFromApi model id msg =
    let cmd = async {
        let! author = bookstoreApi.getAuthor id
        return (msg author)
    }
    { model with CurrentPage = Page.Loading }, Cmd.OfAsync.result cmd
    
let init (currentUrl: Url) : Model * Cmd<Msg> =
    let modelWithUrl = { CurrentUrl = currentUrl; CurrentPage = Page.NotFound }
    match currentUrl with
    | Url.Authorslist ->
        let model, cmd = Authorslist.init ()
        { modelWithUrl with CurrentPage = Page.Authorslist model}, Cmd.map AuthorslistMsg cmd
    | Url.CreateAuthor ->
        let model, cmd = CreateAuthor.init ()
        { modelWithUrl with CurrentPage = Page.CreateAuthor model}, Cmd.map CreateAuthorMsg cmd
    | Url.AuthorDetails id ->
        loadAuthorFromApi modelWithUrl id Msg.GotAuthorForDetails
    | Url.NotFound ->
        modelWithUrl, Cmd.none

let updateCreateAuthorModel msg createModel model =
    let updatedCreateModel, cmd, intent = CreateAuthor.update msg createModel
    let updatedModel = { model with CurrentPage = Page.CreateAuthor updatedCreateModel }
    match intent with
    | CreateAuthor.Intent.ShowAddedAuthor author ->
        updatedModel, Cmd.ofMsg (SwitchToAuthorDetails author)
    | _ ->
        updatedModel, Cmd.map CreateAuthorMsg cmd

let updateAuthorDetailsModel msg authorDetailsModel model =
    let updatedDetailsModel, cmd = AuthorDetails.update msg authorDetailsModel
    { model with CurrentPage = Page.AuthorDetails updatedDetailsModel }, Cmd.map AuthorDetailsMsg cmd
    
let updateAuthorslistModel msg authorslistModel model =
    let updatedAuthorslistModel, cmd, intent = Authorslist.update msg authorslistModel
    let updatedModel = { model with CurrentPage = Page.Authorslist updatedAuthorslistModel}
    match intent with
    | Authorslist.Intent.ShowAuthorDetails author ->
        updatedModel, Cmd.ofMsg (SwitchToAuthorDetails author)
    | Authorslist.Intent.DoNothing ->
        updatedModel, Cmd.map AuthorslistMsg cmd

let update (msg: Msg) (model:Model) :Model * Cmd<Msg> =
    match model.CurrentPage, msg with
    | _, GotAuthorForDetails author ->
        let detailsModel, cmd = AuthorDetails.init author
        { model with CurrentPage = Page.AuthorDetails detailsModel}, Cmd.map AuthorDetailsMsg cmd
    | Page.Authorslist authorModel, AuthorslistMsg authorMsg ->
        updateAuthorslistModel authorMsg authorModel model
    | Page.CreateAuthor authorModel, CreateAuthorMsg authorMsg ->
        updateCreateAuthorModel authorMsg authorModel model
    | Page.AuthorDetails detailsModel, AuthorDetailsMsg detailsMsg ->
        updateAuthorDetailsModel detailsMsg detailsModel model
    | _, SwitchToAuthorDetails author ->
        model, Cmd.navigate("booklist", "authors", author.Id.ToString() )
    | _, _ -> model, Cmd.none

let render (model:Model) (dispatch: Msg -> unit) =
    match model.CurrentPage with
    | Page.Authorslist model -> Authorslist.render model (Msg.AuthorslistMsg>> dispatch)
    | Page.Loading -> Html.div [ Bulma.pageLoader.isActive ]
    | Page.CreateAuthor model -> CreateAuthor.render model (Msg.CreateAuthorMsg >> dispatch)
    | Page.AuthorDetails model -> AuthorDetails.render model (Msg.AuthorDetailsMsg >> dispatch)
    | Page.NotFound -> Html.h1 "Page not found"