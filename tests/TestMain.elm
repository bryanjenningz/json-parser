module TestMain exposing (suite)

import Expect
import Fuzz
import Main exposing (Json(..), json)
import Parser exposing (Problem(..))
import Test exposing (Test)


suite : Test
suite =
    Test.describe "JSON parser"
        [ Test.describe "Parses numbers"
            [ Test.test "Parses 123" <|
                \_ -> Expect.equal (Parser.run json "   123   ") (Ok (JsonNumber 123))
            , Test.test "Parses -1.234" <|
                \_ -> Expect.equal (Parser.run json "  -1.234  ") (Ok (JsonNumber -1.234))
            , Test.fuzz Fuzz.int "Parses any int" <|
                \randomInt ->
                    Expect.equal
                        (Parser.run json (String.fromInt randomInt))
                        (Ok (JsonNumber <| toFloat randomInt))
            , Test.fuzz Fuzz.float "Parses any float" <|
                \randomFloat ->
                    Expect.equal
                        (Parser.run json (String.fromFloat randomFloat))
                        (Ok (JsonNumber randomFloat))
            ]
        , Test.describe "Gives errors for invalid numbers"
            [ Test.test "Gives error for leading zero in number 01" <|
                \_ ->
                    Expect.equal (Parser.run json "01")
                        (Err [ { col = 2, problem = ExpectingEnd, row = 1 } ])
            , Test.fuzz Fuzz.int "Gives error for any leading zero in a number" <|
                \randomInt ->
                    Expect.equal
                        (Parser.run json ("0" ++ String.fromInt randomInt))
                        (Err [ { col = 2, problem = ExpectingEnd, row = 1 } ])
            ]
        , Test.describe "Parses null"
            [ Test.test "Parsers out null with spaces around it" <|
                \_ -> Expect.equal (Parser.run json "   null   ") (Ok JsonNull)
            ]
        , Test.describe "Parses strings" <|
            [ Test.test "Parsers out null with spaces around it" <|
                \_ ->
                    Expect.equal
                        (Parser.run json """  "We're in \\"Elm World\\", aren't we?\u{000D}\t"  """)
                        (Ok (JsonString "We're in \"Elm World\", aren't we?\u{000D}\t"))
            ]
        , Test.describe "Parses booleans"
            [ Test.test "Parses true" <|
                \_ -> Expect.equal (Parser.run json "   true ") (Ok (JsonBoolean True))
            , Test.test "Parses false" <|
                \_ -> Expect.equal (Parser.run json "   false ") (Ok (JsonBoolean False))
            ]
        ]
