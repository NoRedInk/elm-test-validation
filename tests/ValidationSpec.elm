module ValidationSpec exposing (..)

import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Validation
import Validation.Expect


example =
    Validation.create
        [ Validation.describe "hello"
            [ Validation.test "hello is required" <|
                \value ->
                    value.hello
                        |> String.isEmpty
                        |> Validation.Expect.false
            , Validation.test "hello must be shorter than 10 characters" <|
                \value ->
                    value.hello
                        |> String.length
                        |> Validation.Expect.atMost 10
            ]
        , Validation.describe "password confirmation"
            [ Validation.test "password must match confirmation" <|
                \value ->
                    Validation.Expect.equal value.password value.passwordConfirmation
            ]
        ]


tests : Test
tests =
    test "example works" <|
        \() ->
            { hello = "1234567890A"
            , password = "a"
            , passwordConfirmation = "b"
            }
                |> Validation.validate example
                |> Expect.equalDicts
                    (Dict.fromList
                        [ ( "hello", [ "hello must be shorter than 10 characters" ] )
                        , ( "password confirmation", [ "password must match confirmation" ] )
                        ]
                    )
