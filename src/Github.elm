module Github exposing (Issue, Repo, getIssue, getRepo)

import Http
import Json.Decode as D exposing (Decoder)
import Url.Builder



-- TYPES


type alias Repo =
    { name : String
    , description : Maybe String
    , language : Maybe String
    , owner : String
    , fork : Int
    , star : Int
    , watch : Int
    }


type alias Issue =
    { number : Int
    , title : String
    , state : String
    }



-- DECORDER


reposDecoder : Decoder (List Repo)
reposDecoder =
    D.list repoDecoder


repoDecoder : Decoder Repo
repoDecoder =
    D.map7 Repo
        (D.field "name" D.string)
        (D.maybe (D.field "description" D.string))
        (D.maybe (D.field "language" D.string))
        (D.at [ "owner", "login" ] D.string)
        (D.field "forks_count" D.int)
        (D.field "stargazers_count" D.int)
        (D.field "watchers_count" D.int)


issuesDecorder : Decoder (List Issue)
issuesDecorder =
    D.list issueDecoder


issueDecoder : Decoder Issue
issueDecoder =
    D.map3 Issue
        (D.field "number" D.int)
        (D.field "title" D.string)
        (D.field "state" D.string)



-- API


getRepo : (Result Http.Error (List Repo) -> msg) -> String -> Cmd msg
getRepo toMsg userName =
    Http.get
        { url =
            Url.Builder.crossOrigin "https://api.github.com"
                [ "users", userName, "repos" ]
                []
        , expect =
            Http.expectJson toMsg reposDecoder
        }


getIssue : (Result Http.Error (List Issue) -> msg) -> String -> String -> Cmd msg
getIssue toMsg userName repoName =
    Http.get
        { url =
            Url.Builder.crossOrigin "https://api.github.com"
                [ "repos", userName, repoName, "issues" ]
                []
        , expect =
            Http.expectJson toMsg issuesDecorder
        }
