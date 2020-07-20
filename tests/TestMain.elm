module TestMain exposing (suite)

import Dict
import Expect
import Fuzz
import Main exposing (Json(..), jsonParser)
import Parser exposing (Problem(..))
import Test exposing (Test)


suite : Test
suite =
    Test.describe "JSON parser"
        [ Test.describe "Parses numbers"
            [ Test.test "Parses 123" <|
                \_ -> Expect.equal (Parser.run jsonParser "   123   ") (Ok (JsonNumber 123))
            , Test.test "Parses -1.234" <|
                \_ -> Expect.equal (Parser.run jsonParser "  -1.234  ") (Ok (JsonNumber -1.234))
            , Test.fuzz Fuzz.int "Parses any int" <|
                \randomInt ->
                    Expect.equal
                        (Parser.run jsonParser (String.fromInt randomInt))
                        (Ok (JsonNumber <| toFloat randomInt))
            , Test.fuzz Fuzz.float "Parses any float" <|
                \randomFloat ->
                    Expect.equal
                        (Parser.run jsonParser (String.fromFloat randomFloat))
                        (Ok (JsonNumber randomFloat))
            ]
        , Test.describe "Gives errors for invalid numbers"
            [ Test.test "Gives error for leading zero in number 01" <|
                \_ ->
                    Expect.equal (Parser.run jsonParser "01")
                        (Err [ { col = 2, problem = ExpectingEnd, row = 1 } ])
            , Test.fuzz Fuzz.int "Gives error for any leading zero in a number" <|
                \randomInt ->
                    Expect.equal
                        (Parser.run jsonParser ("0" ++ String.fromInt randomInt))
                        (Err [ { col = 2, problem = ExpectingEnd, row = 1 } ])
            ]
        , Test.describe "Parses null"
            [ Test.test "Parsers out null with spaces around it" <|
                \_ -> Expect.equal (Parser.run jsonParser "   null   ") (Ok JsonNull)
            ]
        , Test.describe "Parses strings" <|
            [ Test.test "Parsers out null with spaces around it" <|
                \_ ->
                    Expect.equal
                        (Parser.run jsonParser """  "We're in \\"Elm World\\", aren't we?\u{000D}\t"  """)
                        (Ok (JsonString "We're in \"Elm World\", aren't we?\u{000D}\t"))
            ]
        , Test.describe "Parses booleans"
            [ Test.test "Parses true" <|
                \_ ->
                    Expect.equal (Parser.run jsonParser "   true ")
                        (Ok (JsonBoolean True))
            , Test.test "Parses false" <|
                \_ ->
                    Expect.equal (Parser.run jsonParser "   false ")
                        (Ok (JsonBoolean False))
            ]
        , Test.describe "Parses arrays and objects"
            [ Test.test "Parses array of values" <|
                \_ ->
                    Expect.equal
                        (Parser.run jsonParser "[ 1 , null, \"hi\" ] ")
                        (Ok (JsonArray [ JsonNumber 1, JsonNull, JsonString "hi" ]))
            , Test.test "Parses deeply nested array of values" <|
                \_ ->
                    Expect.equal
                        (Parser.run jsonParser "[ 1 , [[null], [], [[[\"\"]]]], \"hi\" ] ")
                        (Ok
                            (JsonArray
                                [ JsonNumber 1
                                , JsonArray
                                    [ JsonArray [ JsonNull ]
                                    , JsonArray []
                                    , JsonArray [ JsonArray [ JsonArray [ JsonString "" ] ] ]
                                    ]
                                , JsonString "hi"
                                ]
                            )
                        )
            , Test.test "Parses deeply nested objects" <|
                \_ ->
                    Expect.equal
                        (Parser.run
                            jsonParser
                            """
                                {
                                    "a": { "a2": 1 },
                                    "b": 2,
                                    "c": { "d": "abc", "e": {} }
                                }
                            """
                        )
                        (Ok
                            (JsonObject
                                (Dict.fromList
                                    [ ( "a", JsonObject (Dict.fromList [ ( "a2", JsonNumber 1 ) ]) )
                                    , ( "b", JsonNumber 2 )
                                    , ( "c", JsonObject (Dict.fromList [ ( "d", JsonString "abc" ), ( "e", JsonObject (Dict.fromList []) ) ]) )
                                    ]
                                )
                            )
                        )
            , Test.test "Parses deeply nested object and arrays" <|
                \_ ->
                    Expect.equal
                        (Parser.run
                            jsonParser
                            """{"a\\n": 0, "b\\" 嗨": [1, null, true, "", false, [{}], [[1], 2, []]]}"""
                        )
                        (Ok
                            (JsonObject
                                (Dict.fromList
                                    [ ( "a\n", JsonNumber 0 )
                                    , ( "b\" 嗨"
                                      , JsonArray
                                            [ JsonNumber 1
                                            , JsonNull
                                            , JsonBoolean True
                                            , JsonString ""
                                            , JsonBoolean False
                                            , JsonArray [ JsonObject (Dict.fromList []) ]
                                            , JsonArray [ JsonArray [ JsonNumber 1 ], JsonNumber 2, JsonArray [] ]
                                            ]
                                      )
                                    ]
                                )
                            )
                        )
            ]
        ]
