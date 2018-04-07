module Orders exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List exposing (length, append, filter)
import Result.Extra exposing (isOk)
import Http
import Json.Decode as Decode
import Task


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type OrderType
    = Eggs
    | Honey


orderTypeToString : OrderType -> String
orderTypeToString order =
    case order of
        Eggs ->
            "Eggs"

        Honey ->
            "Honey"


type alias Order =
    { id : Int
    , name : String
    , orderType : OrderType
    , datePlaced : String
    }


type alias Model =
    { newName : String
    , newOrderType : Maybe OrderType
    , orders : List Order
    }


init : ( Model, Cmd Msg )
init =
    ( Model "" Nothing [], loadOrders Load )



-- UPDATE


type Msg
    = RequestAdd
    | ConfirmAdd Order
    | Load (Result Http.Error (List Order))
    | UpdateName String
    | ToggleOrderType OrderType


validateNewOrder : String -> Maybe OrderType -> Result () ( String, OrderType )
validateNewOrder name orderType =
    case ( name, orderType ) of
        ( "", _ ) ->
            Err ()

        ( _, Nothing ) ->
            Err ()

        ( name, Just orderType ) ->
            Ok ( name, orderType )


canAddOrder : String -> Maybe OrderType -> Bool
canAddOrder name orderType =
    validateNewOrder name orderType |> isOk


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RequestAdd ->
            case validateNewOrder model.newName model.newOrderType of
                Err () ->
                    ( model, Cmd.none )

                Ok ( newName, newOrderType ) ->
                    ( model, requestAddOrder newName newOrderType )

        ConfirmAdd addedOrder ->
            ( Model "" model.newOrderType (append model.orders [ addedOrder ]), Cmd.none )

        Load (Ok loadedOrders) ->
            ( { model | orders = loadedOrders }, Cmd.none )

        Load (Err _) ->
            ( model, Cmd.none )

        UpdateName updatedName ->
            ( { model | newName = updatedName }, Cmd.none )

        ToggleOrderType newOrderType ->
            case (Just newOrderType) == model.newOrderType of
                True ->
                    ( { model | newOrderType = Nothing }, Cmd.none )

                False ->
                    ( { model | newOrderType = Just newOrderType }, Cmd.none )


requestAddOrder : String -> OrderType -> Cmd Msg
requestAddOrder orderName orderType =
    Task.succeed (ConfirmAdd { id = 1, name = orderName, orderType = orderType, datePlaced = "2018-01-01" })
        |> Task.perform identity


loadOrders : (Result Http.Error (List Order) -> msg) -> Cmd msg
loadOrders callback =
    Http.send callback <| Http.get "http://localhost:5000/api/v1/orders" decodeOrders


decodeOrders : Decode.Decoder (List Order)
decodeOrders =
    Decode.at [ "items" ] (Decode.list decodeOrder)


decodeOrder : Decode.Decoder Order
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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text (toString (length model.orders)) ]
        , ul [] (List.map (\o -> li [] [ text (o.name ++ " " ++ (orderTypeToString o.orderType)) ]) (filterOrders model.orders model.newOrderType))
        , input [ type_ "text", placeholder "Name", value model.newName, onInput UpdateName ] []
        , orderButton model.newOrderType Eggs
        , orderButton model.newOrderType Honey
        , button [ onClick RequestAdd, disabled (not <| canAddOrder model.newName model.newOrderType) ] [ text "Add" ]
        ]


filterOrders : List Order -> Maybe OrderType -> List Order
filterOrders allOrders maybeSelectedOrderType =
    case maybeSelectedOrderType of
        Nothing ->
            allOrders

        Just selectedOrderType ->
            allOrders |> filter (\o -> o.orderType == selectedOrderType)


orderButton : Maybe OrderType -> OrderType -> Html Msg
orderButton existingOrderType orderType =
    button
        [ onClick (ToggleOrderType orderType)
        , style (orderButtonStyle existingOrderType (Just orderType))
        ]
        [ text (orderTypeToString orderType)
        ]


orderButtonStyle : a -> a -> List ( String, String )
orderButtonStyle currentOrderType newOrderType =
    if currentOrderType == newOrderType then
        [ ( "border-color", "red" )
        , ( "border-width", "2px" )
        ]
    else
        []
