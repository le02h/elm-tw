module Parsers.Markdown exposing (parse, render, Token(..), TokenState(..))

import Html exposing (..)

type TokenState
    = Open
    | Close

type Token
    = Heading Int TokenState
    | Text String

-- Parsing

type alias Context =
    { tokens : List Token
    , closeStack : List Token
    , column : Int
    }

parse : String -> List Token
parse str =
    let
        ctx =
            { tokens = []
            , closeStack = []
            , column = 0
            }
    in
    String.foldl tokenize ctx str
    |> .tokens
    |> List.reverse

tokenize : Char -> Context -> Context
tokenize char ctx =
    case char of
        '#' ->
            { ctx | tokens = Heading 1 Open :: ctx.tokens }

        _ ->
            appendText ctx (String.fromChar char)

appendText : Context -> String -> Context
appendText ctx str =
    case ctx.tokens of
        Text t :: xs ->
            { ctx | tokens = Text (t ++ str) :: xs }

        _ ->
            { ctx | tokens = Text str :: ctx.tokens }


-- Rendering

render : String -> Html msg
render str =
    div [] [text "NotImplemented"]
