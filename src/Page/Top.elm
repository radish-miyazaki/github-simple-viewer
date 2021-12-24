module Page.Top exposing (..)

import Github exposing (User)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Url.Builder


type alias Model =
    { input : String
    , state : State
    }


type State
    = Init
    | Waiting
    | Loaded (List User)
    | Error Http.Error


init : Model
init =
    Model "" Init


type Msg
    = Input String
    | Send
    | Receive (Result Http.Error (List User))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input newInput ->
            ( { model | input = newInput }, Cmd.none )

        Send ->
            ( { model | input = "", state = Waiting }
            , Github.searchUsers Receive model.input
            )

        Receive (Ok users) ->
            ( { model | state = Loaded users }, Cmd.none )

        Receive (Err err) ->
            ( { model | state = Error err }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ Html.form [ onSubmit Send ]
            [ input
                [ onInput Input
                , autofocus True
                , placeholder "Github name"
                , value model.input
                ]
                []
            , button [ type_ "submit", disabled (not (isValidInput model.input)) ] [ text "Send" ]
            ]
        , case model.state of
            Init ->
                text ""

            Waiting ->
                text "Waiting..."

            Loaded users ->
                viewUsers users

            Error e ->
                case e of
                    Http.BadBody message ->
                        pre [] [ text message ]

                    _ ->
                        text (Debug.toString e)
        ]


isValidInput : String -> Bool
isValidInput input =
    String.length input >= 1


viewUsers : List User -> Html Msg
viewUsers users =
    if List.length users == 0 then
        text "No user."

    else
        ul [] (List.map viewUser users)


viewUser : User -> Html Msg
viewUser user =
    viewLink (Url.Builder.absolute [ user.login ] [])


viewLink : String -> Html Msg
viewLink path =
    li [] [ a [ href path ] [ text path ] ]
