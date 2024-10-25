module Parsers.Markdown exposing (Token(..), TokenState(..), parse, render)

import Html exposing (..)


{--
This a Markdown parser based on a Finite State Machine (FSM) approach.
--}

-- General Types

type TokenState
    = OpenTag
    | CloseTag
    | SelfClosedTag

type Token
    = Heading Int TokenState
    | Text String

-- Parsing

type State
    = Start
    | Block
    | Inline
    | Heading
    | Text


type alias Context =
    { state :State
    , column : Int
    , offset : Int
    , tokens : List Token
    , stack : List Token
    }

parse : String -> List Token
parse str =
    let
        ctx : Context
        ctx =
            { state = Start
            , column = 0
            , offset = 0
            , tokens = []
            , stack = []
            }
    in
    String.foldl fsm ctx str
        |> .tokens


updateContext : Context -> Char -> Context
updateContext ctx char =
    case char of
        '\n' ->
            { ctx | column = 0, offset = ctx.offset + 1 }

        _ ->
            { ctx | column = ctx.column + 1, offset = ctx.offset + 1 }


fsm : Context -> Char -> Context
fsm ctx' char =
    let
        ctx = updateContext ctx' char
    in
    -- Transition Table
    case (ctx.state, char) of
        (Start, '#') ->



-- Rendering


render : String -> Html msg
render _ =
    div [] [ text "NotImplemented" ]
