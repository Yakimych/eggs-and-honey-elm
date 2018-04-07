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
        , describe "filterOrders"
            [ test "returns all orders if no order is selected" <|
                \_ ->
                    let
                        orderList =
                            [ { id = 1, name = "TestName", orderType = Eggs, datePlaced = "2015-01-01" }
                            , { id = 2, name = "TestName2", orderType = Honey, datePlaced = "2014-01-01" }
                            ]
                    in
                        Expect.equalLists orderList (filterOrders orderList Nothing)
            , test "returns only orders for Honey if Honey is selected" <|
                \_ ->
                    let
                        orderList =
                            [ { id = 1, name = "TestName", orderType = Eggs, datePlaced = "2015-01-01" }
                            , { id = 2, name = "TestName2", orderType = Honey, datePlaced = "2014-01-01" }
                            , { id = 3, name = "TestName3", orderType = Eggs, datePlaced = "2014-01-01" }
                            , { id = 4, name = "TestName4", orderType = Honey, datePlaced = "2014-01-01" }
                            ]
                    in
                        Expect.true "Expected only to find orders for Honey in the list" ((filterOrders orderList (Just Honey)) |> List.all (\o -> o.orderType == Honey))
            , test "returns only orders for Eggs if Eggs are selected" <|
                \_ ->
                    let
                        orderList =
                            [ { id = 1, name = "TestName", orderType = Eggs, datePlaced = "2015-01-01" }
                            , { id = 2, name = "TestName2", orderType = Honey, datePlaced = "2014-01-01" }
                            , { id = 3, name = "TestName3", orderType = Eggs, datePlaced = "2014-01-01" }
                            , { id = 4, name = "TestName4", orderType = Honey, datePlaced = "2014-01-01" }
                            , { id = 5, name = "TestName5", orderType = Eggs, datePlaced = "2014-01-01" }
                            ]
                    in
                        Expect.true "Expected only to find orders for Eggs in the list" ((filterOrders orderList (Just Eggs)) |> List.all (\o -> o.orderType == Eggs))
            ]
        ]
