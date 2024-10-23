module MarkdownParse exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Parsers.Markdown exposing (parse, Token(..), TokenState(..))


suite : Test
suite =
    describe "Parsers.Markdown.parse"
        [ test "parses '# Hello, World!'" <|
            \_ ->
                parse "# Hello, World!"
                |> Expect.equal [ Heading 1 Open, Text "Hello, World!" ]
        ]
