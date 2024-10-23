module Main exposing (..)

import TextType.Markdown.Parser exposing (parse)

main =
    Debug.log (parse "# Hello, world!")
