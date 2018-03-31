module OrderTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Orders exposing (..)


suite : Test
suite =
    describe
        "The Orders module"
        [ describe "canAddOrder"
            [ fuzz string "returns False if order is Nothing" <|
                \randomName -> Expect.false "Expected to not be able to add the order with empty Order" (canAddOrder randomName Nothing)

            -- TODO: Make those into fuzz tests too
            , test "returns False if name is empty string" <|
                \_ -> Expect.false "Expected to not be able to add the order with empty Name" (canAddOrder "" (Just Eggs))
            , test "returns True if name is empty string" <|
                \_ -> Expect.true "Expected to not be able to add the order with empty Name" (canAddOrder "SomeName" (Just Honey))
            ]
        ]
