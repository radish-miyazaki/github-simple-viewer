module Page.Repo exposing (..)

import Github exposing (Issue, getIssues, issueUrl)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http


type alias Model =
    { userName : String
    , repoName : String
    , state : State
    }


type State
    = Init
    | Loaded (List Issue)
    | Error Http.Error


init : String -> String -> ( Model, Cmd Msg )
init userName repoName =
    ( Model userName repoName Init
    , getIssues GotIssues userName repoName
    )


type Msg
    = GotIssues (Result Http.Error (List Issue))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotIssues (Ok issues) ->
            ( { model | state = Loaded issues }, Cmd.none )

        GotIssues (Err err) ->
            ( { model | state = Error err }, Cmd.none )


view : Model -> Html Msg
view model =
    case model.state of
        Init ->
            text "Loading..."

        Loaded issues ->
            if List.length issues == 0 then
                text "No issue."

            else
                ul [] (List.map (viewIssue model.userName model.repoName) issues)

        Error e ->
            text (Debug.toString e)


viewIssue : String -> String -> Issue -> Html Msg
viewIssue userName repoName issue =
    li []
        [ span [] [ text ("[" ++ issue.state ++ "]") ]
        , a
            [ href (issueUrl userName repoName issue.number)
            , target "_blank"
            ]
            [ text ("#" ++ String.fromInt issue.number)
            , text issue.title
            ]
        ]
