module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Github exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Page.Repo
import Page.Top
import Page.User
import Route exposing (Route(..))
import Url
import Url.Parser exposing (..)


type Page
    = NotFound
    | ErrorPage Http.Error
    | TopPage Page.Top.Model
    | UserPage Page.User.Model
    | RepoPage Page.Repo.Model



-- MODEL


type alias Model =
    { key : Nav.Key
    , page : Page
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    -- 後で画面遷移で使うためのキーをModelにもたせておく
    Model key (TopPage Page.Top.init)
        -- はじめてページを訪れたときもページの初期化を行う
        |> goTo (Route.parse url)


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
    | TopMsg Page.Top.Msg
    | UserMsg Page.User.Msg
    | RepoMsg Page.Repo.Msg



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

        TopMsg topMsg ->
            case model.page of
                TopPage topModel ->
                    let
                        ( newTopModel, topCmd ) =
                            Page.Top.update topMsg topModel
                    in
                    ( { model | page = TopPage newTopModel }
                    , Cmd.map TopMsg topCmd
                    )

                _ ->
                    ( model, Cmd.none )

        UserMsg userMsg ->
            case model.page of
                UserPage userModel ->
                    let
                        ( newUserModel, topCmd ) =
                            Page.User.update userMsg userModel
                    in
                    ( { model | page = UserPage newUserModel }
                    , Cmd.map UserMsg topCmd
                    )

                _ ->
                    ( model, Cmd.none )

        RepoMsg repoMsg ->
            case model.page of
                RepoPage repoModel ->
                    let
                        ( newRepoModel, topCmd ) =
                            Page.Repo.update repoMsg repoModel
                    in
                    ( { model | page = RepoPage newRepoModel }
                    , Cmd.map RepoMsg topCmd
                    )

                _ ->
                    ( model, Cmd.none )



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

            TopPage topPageModel ->
                Page.Top.view topPageModel
                    |> Html.map TopMsg

            UserPage userPageModel ->
                Page.User.view userPageModel
                    |> Html.map UserMsg

            RepoPage repoPageModel ->
                Page.Repo.view repoPageModel
                    |> Html.map RepoMsg
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


goTo : Maybe Route -> Model -> ( Model, Cmd Msg )
goTo maybeRoute model =
    case maybeRoute of
        Nothing ->
            ( { model | page = NotFound }, Cmd.none )

        Just Route.Top ->
            ( { model | page = TopPage Page.Top.init }, Cmd.none )

        Just (Route.User userName) ->
            let
                ( userModel, userCmd ) =
                    Page.User.init userName
            in
            ( { model | page = UserPage userModel }
            , Cmd.map UserMsg userCmd
            )

        Just (Route.Repo userName repoName) ->
            let
                ( repoModel, repoCmd ) =
                    Page.Repo.init userName repoName
            in
            ( { model | page = RepoPage repoModel }
            , Cmd.map RepoMsg repoCmd
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
