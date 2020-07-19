module Main exposing (main)

import Dict exposing (Dict)
import Html exposing (Html, text)
import Parser exposing ((|.), (|=), Parser, Step(..))


type Json
    = JsonNull
    | JsonBoolean Bool
    | JsonNumber Float
    | JsonString String
    | JsonArray (List Json)
    | JsonObject (Dict String Json)


bool : Parser Bool
bool =
    Parser.oneOf
        [ Parser.keyword "true" |> Parser.map (\_ -> True)
        , Parser.keyword "false" |> Parser.map (\_ -> False)
        ]


nonDoubleQuote : Parser String
nonDoubleQuote =
    Parser.chompWhile ((/=) '"')
        |> Parser.getChompedString


string : Parser String
string =
    Parser.succeed identity
        |. Parser.token "\""
        |= nonDoubleQuote
        |. Parser.token "\""


arrayEntries : Parser (List Json)
arrayEntries =
    Parser.oneOf
        [ Parser.succeed (::)
            |. Parser.spaces
            |= json
            |. Parser.spaces
            |= Parser.loop [] collectArrayEntries
        , Parser.succeed ()
            |> Parser.map (\_ -> [])
        ]


collectArrayEntries : List Json -> Parser (Step (List Json) (List Json))
collectArrayEntries reversedTerms =
    Parser.oneOf
        [ Parser.succeed (\term -> Loop (term :: reversedTerms))
            |. Parser.spaces
            |. Parser.token ","
            |. Parser.spaces
            |= json
            |. Parser.spaces
        , Parser.succeed ()
            |> Parser.map (\_ -> Done (List.reverse reversedTerms))
        ]


objectEntry : Parser (Dict String Json)
objectEntry =
    Parser.succeed (\key val -> Dict.fromList [ ( key, val ) ])
        |. Parser.spaces
        |= string
        |. Parser.spaces
        |. Parser.token ":"
        |. Parser.spaces
        |= json
        |. Parser.spaces


collectObjectEntries : Dict String Json -> Parser (Step (Dict String Json) (Dict String Json))
collectObjectEntries entries =
    Parser.oneOf
        [ Parser.succeed (\entry -> Loop (Dict.union entries entry))
            |. Parser.spaces
            |. Parser.token ","
            |. Parser.spaces
            |= objectEntry
            |. Parser.spaces
        , Parser.succeed ()
            |> Parser.map (\_ -> Done entries)
        ]


objectEntries : Parser Json
objectEntries =
    Parser.oneOf
        [ Parser.succeed
            (\entry rest -> JsonObject (Dict.union entry rest))
            |= objectEntry
            |= Parser.loop Dict.empty collectObjectEntries
        , Parser.succeed ()
            |> Parser.map (\_ -> JsonObject Dict.empty)
        ]


jsonNull : Parser Json
jsonNull =
    Parser.keyword "null" |> Parser.map (\_ -> JsonNull)


jsonBoolean : Parser Json
jsonBoolean =
    bool |> Parser.map JsonBoolean


jsonNumber : Parser Json
jsonNumber =
    Parser.float |> Parser.map JsonNumber


jsonString : Parser Json
jsonString =
    string |> Parser.map JsonString


jsonArray : Parser Json
jsonArray =
    Parser.succeed JsonArray
        |. Parser.token "["
        |. Parser.spaces
        |= Parser.lazy (\_ -> arrayEntries)
        |. Parser.spaces
        |. Parser.token "]"


jsonObject : Parser Json
jsonObject =
    Parser.succeed identity
        |. Parser.token "{"
        |. Parser.spaces
        |= Parser.lazy (\_ -> objectEntries)
        |. Parser.spaces
        |. Parser.token "}"


json : Parser Json
json =
    Parser.oneOf
        [ jsonNull
        , jsonBoolean
        , jsonNumber
        , jsonString
        , jsonArray
        , jsonObject
        ]


main : Html msg
main =
    text << Debug.toString <| Parser.run json """{"a": 1, "b": [1, null, true, false, [{}], []]}"""
