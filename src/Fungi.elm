module Fungi exposing (..)

import Html exposing (a)
import Levenshtein
import Parser exposing ((|.), (|=), DeadEnd, Parser, Step(..), Trailing(..))
import Path exposing (Path)


parseFile : Path -> String -> Result (List DeadEnd) PartialModule
parseFile entryPath =
    Parser.run (parsePartialModule entryPath)


type alias PartialModule =
    { imports : List Import
    , body : String
    }



---- FILES ----


parsePartialModule : Path -> Parser PartialModule
parsePartialModule entryPath =
    Parser.succeed PartialModule
        |= parseImports entryPath
        |= (Parser.chompWhile (\_ -> True) |> Parser.getChompedString)


parseImports : Path -> Parser (List Import)
parseImports entryPath =
    Parser.loop [] (parseImportsHelper entryPath)


parseImportsHelper : Path -> List Import -> Parser (Parser.Step (List Import) (List Import))
parseImportsHelper entryPath reversedImports =
    Parser.oneOf
        [ Parser.succeed (\import_ -> Loop (import_ :: reversedImports))
            |= parseImport entryPath
            |> Parser.backtrackable
        , Parser.succeed ()
            |> Parser.map (\() -> Done (List.reverse reversedImports))
        ]


partialModuleToString : PartialModule -> String
partialModuleToString partialModule =
    "Partial Module:\n  Imports:"
        ++ (case partialModule.imports of
                [] ->
                    " None"

                _ ->
                    "\n"
                        ++ (List.map (importToString >> (++) "    ") partialModule.imports |> String.join "\n")
                        ++ "\n"
                        ++ partialModule.body
           )


type alias Import =
    { path : String
    , alias_ : String
    }


parseImport : Path -> Parser Import
parseImport entryPath =
    Parser.succeed Import
        |. Parser.spaces
        |. (Parser.chompWhile (\char -> char /= '"' && (not <| isSpaceChar char))
                |> Parser.getChompedString
                |> Parser.andThen
                    (\importWord ->
                        if Levenshtein.distance "import" importWord <= 4 then
                            Parser.succeed ()

                        else
                            Parser.problem ("Expected 'import' but found " ++ importWord)
                    )
           )
        |. Parser.spaces
        |= parsePath entryPath
        |. Parser.spaces
        |. Parser.keyword "as"
        |. Parser.spaces
        |= parseImportAlias
        |. Parser.spaces
        |. Parser.symbol ";"


isSpaceChar : Char -> Bool
isSpaceChar char =
    char == ' ' || char == '\t' || char == '\n' || char == '\u{000D}'


parsePath : Path -> Parser String
parsePath entryPath =
    Parser.succeed (Path.join (Path.getFileDirectory entryPath))
        |. Parser.symbol "\""
        |= (Parser.succeed ()
                |. Parser.chompIf ((/=) '"')
                |. Parser.chompWhile ((/=) '"')
                |> Parser.getChompedString
           )
        |. Parser.symbol "\""


parseImportAlias : Parser String
parseImportAlias =
    Parser.succeed ()
        |. Parser.chompIf (\char -> Char.isAlpha char && Char.isUpper char)
        |. Parser.chompWhile ((/=) ';')
        |> Parser.getChompedString


importToString : Import -> String
importToString import_ =
    "Import from " ++ import_.path ++ " as " ++ import_.alias_



---- MODULES ----


type alias Module =
    { imports : List Import
    , definitions : List Definition
    }


moduleToString : Module -> String
moduleToString module_ =
    "Partial Module:\n  Imports:"
        ++ (case module_.imports of
                [] ->
                    " None"

                _ ->
                    "\n" ++ (List.map (importToString >> (++) "    ") module_.imports |> String.join "\n")
           )
        ++ "\n  Definitions:"
        ++ (case module_.definitions of
                [] ->
                    " None"

                _ ->
                    "\n" ++ (List.map (definitionToString >> (++) "    ") module_.definitions |> String.join "\n")
           )


parseModule : PartialModule -> Result (List DeadEnd) Module
parseModule partialModule =
    case Parser.run parseBody partialModule.body of
        Err err ->
            Err err

        Ok body ->
            Ok { imports = partialModule.imports, definitions = body }


parseBody : Parser (List Definition)
parseBody =
    Parser.loop [] parseBodyHelper


parseBodyHelper : List Definition -> Parser (Step (List Definition) (List Definition))
parseBodyHelper definitions =
    Parser.oneOf
        [ Parser.succeed (\definition -> Loop (definition :: definitions))
            |= parseDefinition
        , Parser.succeed (Done definitions)
        ]


type alias Definition =
    { docComment : Maybe String
    , markers : Maybe (List String)
    , label : String
    , body : String
    }


definitionToString : Definition -> String
definitionToString definition =
    "Definition(" ++ markersToString definition.markers ++ "):" ++ docCommentToString definition.docComment ++ "\n     " ++ definition.label ++ " = " ++ definition.body


docCommentToString : Maybe String -> String
docCommentToString =
    Maybe.withDefault ""


markersToString : Maybe (List String) -> String
markersToString maybeMarkers =
    case maybeMarkers of
        Nothing ->
            ""

        Just markers ->
            String.join ", " markers


parseDefinition : Parser Definition
parseDefinition =
    Parser.succeed Definition
        |. Parser.spaces
        |= parseDocComment
        |. Parser.spaces
        |= parseMarkers
        |. Parser.spaces
        |= parseDefinitionLabel
        |. Parser.spaces
        |. Parser.symbol "="
        |= parseDefinitionBody



-- |. Parser.symbol ";"


parseDefinitionBody : Parser String
parseDefinitionBody =
    Parser.succeed ()
        |. Parser.chompIf ((/=) ';')
        |. Parser.chompWhile ((/=) ';')
        |> Parser.getChompedString


parseDefinitionLabel : Parser String
parseDefinitionLabel =
    Parser.succeed ()
        |. Parser.chompIf (\char -> Char.isAlpha char && Char.isLower char)
        |. Parser.chompWhile (\char -> Char.isAlphaNum char || char == '-' || char == '_')
        |> Parser.getChompedString


parseMarkers : Parser (Maybe (List String))
parseMarkers =
    Parser.oneOf
        [ Parser.succeed Just
            |. Parser.symbol "#"
            |= Parser.sequence
                { start = "["
                , item = parseMarker
                , spaces = Parser.spaces
                , separator = ","
                , trailing = Optional
                , end = "]"
                }
            |. Parser.spaces
            |> Parser.backtrackable
        , Parser.succeed Nothing
        ]


parseMarker : Parser String
parseMarker =
    Parser.succeed ()
        |. Parser.chompIf (\char -> Char.isAlpha char && Char.isLower char)
        |. Parser.chompWhile Char.isAlpha
        |> Parser.getChompedString


parseDocComment : Parser (Maybe String)
parseDocComment =
    Parser.oneOf
        [ Parser.succeed Just
            |. Parser.spaces
            |. Parser.symbol "#|"
            |= (Parser.chompUntil "|#" |> Parser.getChompedString)
            |. Parser.symbol "|#"
            |> Parser.backtrackable
        , Parser.succeed Nothing
        ]


parseLineComment : Parser ()
parseLineComment =
    Parser.lineComment "#"



---- EXPRESSIONS ----
