module Route exposing (Route(..), parse)

import Url exposing (Url)
import Url.Parser exposing (..)



-- TYPE


type Route
    = Top
    | User String
    | Repo String String



-- PARSE


parse : Url -> Maybe Route
parse url =
    Url.Parser.parse parser url



-- PARSER


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Top top
        , map User string
        , map Repo (string </> string)
        ]
