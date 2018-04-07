module DataProvider exposing (loadOrders)

import Model exposing (OrderType(..), Order)
import Http
import Json.Decode as Decode


loadOrders : (Result Http.Error (List Model.Order) -> msg) -> Cmd msg
loadOrders callback =
    Http.send callback <| Http.get "http://localhost:5000/api/v1/orders" decodeOrders


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
