module Github exposing (Issue, Repo, User, getIssues, getRepos, issueUrl, searchUsers)

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


type alias User =
    { login : String
    , avatarUrl : String
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


usersDecorder : Decoder (List User)
usersDecorder =
    D.list userDecorder


userDecorder : Decoder User
userDecorder =
    D.map2 User
        (D.field "login" D.string)
        (D.field "avatar_url" D.string)


searchUsersDecorder : Decoder (List User)
searchUsersDecorder =
    D.field "items" usersDecorder



-- API


searchUsers : (Result Http.Error (List User) -> msg) -> String -> Cmd msg
searchUsers toMsg userName =
    Http.get
        { url =
            Url.Builder.crossOrigin "https://api.github.com"
                [ "search", "users" ]
                [ Url.Builder.string "q" userName ]
        , expect =
            Http.expectJson toMsg searchUsersDecorder
        }


getRepos : (Result Http.Error (List Repo) -> msg) -> String -> Cmd msg
getRepos toMsg userName =
    Http.get
        { url =
            Url.Builder.crossOrigin "https://api.github.com"
                [ "users", userName, "repos" ]
                []
        , expect =
            Http.expectJson toMsg reposDecoder
        }


getIssues : (Result Http.Error (List Issue) -> msg) -> String -> String -> Cmd msg
getIssues toMsg userName repoName =
    Http.get
        { url =
            Url.Builder.crossOrigin "https://api.github.com"
                [ "repos", userName, repoName, "issues" ]
                []
        , expect =
            Http.expectJson toMsg issuesDecorder
        }


issueUrl : String -> String -> Int -> String
issueUrl userName repoName issueNumber =
    Url.Builder.crossOrigin "https://github.com"
        [ userName, repoName, "issues", String.fromInt issueNumber ]
        []
