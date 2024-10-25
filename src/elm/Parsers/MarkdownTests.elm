module Parsers.MarkdownTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Parsers.Markdown exposing (parse, Token(..), TagType(..))


suite : Test
suite =
    describe "Parse Markdown in a String to a List of Tokens"
        [ describe "Simple CommonMark"
            [ describe "Heading only"
                [ test "parses h1" <|
                    \_ ->
                        parse "# Hello, World!"
                        |> Expect.equal [ Heading 1 OpenTag, Text "Hello, World!", Heading 1 CloseTag ]

                , test "parses h2" <|
                    \_ ->
                        parse "## Hello, World!"
                        |> Expect.equal [ Heading 2 OpenTag, Text "Hello, World!", Heading 2 CloseTag ]

                , test "parses h3" <|
                    \_ ->
                        parse "### Hello, World!"
                        |> Expect.equal [ Heading 3 OpenTag, Text "Hello, World!", Heading 3 CloseTag ]

                ]
            ]
        ]
