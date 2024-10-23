module TextType.Markdown.Parser exposing (AST(..))

type AST
    = Root (List AST)
    | Paragraph (List AST)
    | Heading Int (List AST)

type alias Context =
    { src : String
    , line : Int
    , column : Int
    , offset : Int
    , ast : AST
    }

parse : String -> AST
parse str =
    let
        ctx =
            { src = str
            , line = 1
            , column = 1
            , offset = 0
            , ast = Root []
            }
    in
    parseBlock ctx

parseBlock : Context -> AST
parseBlock ctx =
    let
        c = String.uncons ctx.src
    in
    case c of
        Just (char, rest) ->
            case char of
                '#' ->
                    ctx.ast
                _ ->
                    ctx.ast
        Nothing ->
            ctx.ast