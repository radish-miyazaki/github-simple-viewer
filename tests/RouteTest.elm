module RouteTest exposing (suite)

import Expect exposing (..)
import Fuzz exposing (..)
import Route exposing (..)
import Test exposing (..)
import Url


testParse : String -> String -> Maybe Route -> Test
testParse name path expectRoute =
    test name <|
        \_ ->
            Url.fromString path
                |> Maybe.andThen Route.parse
                |> Expect.equal expectRoute


suite : Test
suite =
    describe "Route"
        [ testParse "should path Top" "/" (Just Route.Top)
        , testParse "should path Top with queries" "/?dummy=value" (Just Route.Top)
        , testParse "should path Top with hash" "/#dummy" (Just Route.Top)
        , testParse "should path User" "/foo" (Just (Route.User "foo"))
        , testParse "should path Repo" "/foo/bar" (Just (Route.Repo "foo" "bar"))
        , testParse "should path invalid path" "/foo/bar/baz" Nothing
        ]
