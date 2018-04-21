module DataProvider exposing (loadOrders, requestAddOrder)

import Model exposing (OrderType(..), Order)
import Http
import Json.Decode as Decode
import Json.Encode as Encode


baseUrl =
    "http://localhost:5000/api/v1/orders"


addOrderUrl =
    baseUrl ++ "/add"


loadOrders : (Result Http.Error (List Model.Order) -> msg) -> Cmd msg
loadOrders callback =
    Http.send callback <| Http.get baseUrl decodeOrders


encodeOrder : String -> OrderType -> Encode.Value
encodeOrder name orderType =
    Encode.object
        [ ( "name", Encode.string name )
        , ( "order", Encode.string (Model.orderTypeToString orderType) )
        ]


requestAddOrder : (Result Http.Error Int -> msg) -> String -> OrderType -> Cmd msg
requestAddOrder callback orderName orderType =
    Http.send callback <|
        Http.post addOrderUrl (Http.jsonBody (encodeOrder orderName orderType)) (Decode.field "id" Decode.int)


decodeOrders : Decode.Decoder (List Model.Order)
decodeOrders =
    Decode.at [ "items" ] (Decode.list decodeOrder)


decodeOrder : Decode.Decoder Model.Order
decodeOrder =
    Decode.map4 Order
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "order" Decode.string
            |> Decode.andThen
                (\str ->
                    case str of
                        "Eggs" ->
                            Decode.succeed Eggs

                        "Honey" ->
                            Decode.succeed Honey

                        somethingElse ->
                            Decode.fail <| "Unknown order type: " ++ somethingElse
                )
        )
        (Decode.field "datePlaced" Decode.string)
