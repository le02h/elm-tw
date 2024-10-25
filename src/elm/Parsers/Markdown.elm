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
    | HeadingState Int
    | TextState


type alias Context =
    { input : String
    , state : State
    , span : ( Int, Int )
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
            ( spanEnd + 1, spanEnd + 1 )

        ctx_ =
            { ctx | span = ( spanStart, spanEnd + 1 ) }

        state_event =
            ( ctx.state, char )
    in
    case state_event of
        -- Transition Table
        ( BlockState, '#' ) -> transitionTo ctx_ (HeadingState 1)
        ( BlockState, _ ) -> ctx_
        ( HeadingState level, '#' ) -> transitionTo ctx_ (HeadingState (level + 1))
        ( HeadingState level, ' ' ) -> headingMatch ctx_ level
        ( HeadingState _, _ ) -> ctx_
        ( InlineState, _ ) -> transitionTo ctx_ TextState
        ( TextState, _ ) -> ctx_


transitionTo : Context -> State -> Context
transitionTo ctx state =
    { ctx | state = state }


headingMatch : Context -> Int -> Context
headingMatch ctx level =
    let
        (_, spanEnd) =
            ctx.span

        nextSpan =
            ( spanEnd, spanEnd )
    in
    if level <= 3 then
        { ctx | state = InlineState
        , tokens = ctx.tokens ++ [ Heading level OpenTag ]
        , stack = Heading level CloseTag :: ctx.stack
        , span = nextSpan
        }
    else
        -- TODO create a paragraph token instead
        { ctx | state = InlineState
        , tokens = ctx.tokens ++ [ Heading level OpenTag ]
        , stack = Heading level CloseTag :: ctx.stack
        , span = nextSpan
        }

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
