module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Github exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Route exposing (Route(..), parse)
import Url exposing (Url)
import Url.Builder
import Url.Parser exposing (..)


type Page
    = NotFound
    | ErrorPage Http.Error
    | TopPage
    | UserPage (List Repo)
    | RepoPage (List Issue)



-- MODEL


type alias Model =
    { key : Nav.Key
    , page : Page
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    -- 後で画面遷移で使うためのキーを　Modelにもたせておく
    Model key TopPage
        -- はじめてページを訪れたときもページの初期化を行う
        |> goTo (Route.parse url)


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
    | Loaded (Result Http.Error Page)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            -- ページの初期化処理をヘルパ関数に移譲
            goTo (Route.parse url) model

        Loaded result ->
            ( { model
                | page =
                    case result of
                        Ok page ->
                            page

                        Err e ->
                            ErrorPage e
              }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "My Github Viewer"
    , body =
        [ a [ href "/" ] [ h1 [] [ text "My Github Viewer" ] ]
        , case model.page of
            NotFound ->
                viewNotFound

            ErrorPage error ->
                viewError error

            TopPage ->
                viewTopPage

            UserPage repos ->
                viewUserPage repos

            RepoPage issues ->
                viewRepoPage issues
        ]
    }



-- HELPERS


viewNotFound : Html msg
viewNotFound =
    text "not found"


viewError : Http.Error -> Html msg
viewError error =
    case error of
        Http.BadBody message ->
            pre [] [ text message ]

        _ ->
            text (Debug.toString error)


viewTopPage : Html msg
viewTopPage =
    ul []
        -- ユーザ名一覧は、とりあえず固定
        [ viewLink (Url.Builder.absolute [ "radish-miyazaki" ] [])
        , viewLink (Url.Builder.absolute [ "mather" ] [])
        ]


viewUserPage : List Repo -> Html msg
viewUserPage repos =
    ul []
        -- ユーザの持っているRepo一覧を取得
        (repos
            |> List.map
                (\repo ->
                    viewLink (Url.Builder.absolute [ repo.owner, repo.name ] [])
                )
        )


viewRepoPage : List Issue -> Html msg
viewRepoPage issues =
    -- リポジトリのIssue一覧を取得
    ul [] (List.map viewIssue issues)


viewIssue : Issue -> Html msg
viewIssue issue =
    li []
        [ span [] [ text ("[" ++ issue.state ++ "]") ]
        , span [] [ text ("#" ++ String.fromInt issue.number ++ "#") ]
        , span [] [ text issue.title ]
        ]


viewLink : String -> Html msg
viewLink path =
    li [] [ a [ href path ] [ text path ] ]


goTo : Maybe Route -> Model -> ( Model, Cmd Msg )
goTo maybeRoute model =
    case maybeRoute of
        Nothing ->
            ( { model | page = NotFound }, Cmd.none )

        Just Route.Top ->
            ( { model | page = TopPage }, Cmd.none )

        Just (Route.User userName) ->
            ( model
            , Github.getRepo (Result.map UserPage >> Loaded) userName
            )

        Just (Route.Repo userName repoName) ->
            ( model
            , Github.getIssue (Result.map RepoPage >> Loaded) userName repoName
            )



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }
