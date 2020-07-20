module TestMain exposing (suite)

import Expect
import Fuzz
import Main exposing (Json(..), json)
import Parser
import Test exposing (Test)


suite : Test
suite =
    Test.describe "JSON parser"
        [ Test.describe "Parses numbers"
            [ Test.test "Parses 123" <|
                \_ -> Expect.equal (Parser.run json "123   ") (Ok (JsonNumber 123))
            , Test.test "Parses -1.234" <|
                \_ -> Expect.equal (Parser.run json "-1.234  ") (Ok (JsonNumber -1.234))
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
        ]
