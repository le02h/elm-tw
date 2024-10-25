module Parsers.Markdown exposing (Token(..), TagType(..), parse, render)

import Html exposing (..)



{--
This a Markdown parser based on a Finite State Machine (FSM) approach.
--}
-- General Types


type TagType
    = OpenTag
    | CloseTag


type Token
    = Heading Int TagType
    | Text String



-- Parsing


type State
    = BlockState
    | InlineState
    | HeadingState
    | TextState


type alias Context =
    { input : String
    , state : State
    , span : ( Int, Int )
    , heading_level : Int
    , tokens : List Token
    , stack : List Token
    }


parse : String -> List Token
parse input =
    let
        ctx : Context
        ctx =
            { input = input
            , state = BlockState
            , span = ( 0, 0 )
            , heading_level = 0
            , tokens = []
            , stack = []
            }
    in
    String.foldl fsm ctx input
        |> finalize
        |> unwrapStack
        |> .tokens


fsm :  Char -> Context -> Context
fsm char ctx =
    let
        (spanStart, spanEnd) =
            ctx.span

        newSpan =
            ( spanEnd + 1, spanEnd + 2 )

        ctx_ =
            { ctx | span = ( spanStart, spanEnd + 1 ) }

        state_event =
            ( ctx.state, char )
    in
    -- Transition Table
    case state_event of
        ( BlockState, '#' ) ->
            { ctx_ | state = HeadingState, heading_level = 1 }

        ( BlockState, _ ) -> ctx_

        ( HeadingState, '#' ) ->
            { ctx_ | heading_level = ctx.heading_level + 1 }

        ( HeadingState, ' ' ) ->
            let
                level = ctx.heading_level
            in
            { ctx_ | state = InlineState
            , tokens = ctx.tokens ++ [ Heading level OpenTag ]
            , stack = Heading level CloseTag :: ctx.stack
            , span = newSpan
            , heading_level = 0 }

        ( HeadingState, _ ) -> ctx_

        ( InlineState, _ ) ->
            { ctx_ | state = TextState }

        ( TextState, '\n') ->
            { ctx_ | state = BlockState
            , tokens = ctx.tokens ++ [ Text (String.slice spanStart spanEnd ctx.input) ]
            , span = newSpan }

        ( TextState, _ ) -> ctx_


finalize : Context -> Context
finalize ctx =
    let
        (spanStart, spanEnd) =
            ctx.span
    in
    case ctx.state of
        TextState ->
            { ctx | tokens = ctx.tokens ++ [ Text (String.slice spanStart spanEnd ctx.input) ] }

        _ -> ctx


unwrapStack : Context -> Context
unwrapStack ctx =
    case ctx.stack of
        token :: xs ->
            { ctx | tokens = ctx.tokens ++ [ token ], stack = xs }

        [] -> ctx


-- Rendering


render : String -> Html msg
render _ =
    div [] [ text "NotImplemented" ]
