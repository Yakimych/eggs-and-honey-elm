module Orders exposing (Model, Msg(..), canAddOrder, filterOrders, init, main, orderButton, orderButtonStyle, subscriptions, update, validateNewOrder, view)

import Browser
import DataProvider exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import List exposing (append, concat, filter, length)
import Model exposing (OrderType(..), orderTypeToString)
import String exposing (fromInt)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    { newName : String
    , newOrderType : Maybe OrderType
    , orders : List Model.Order
    , statusMessage : String
    }


init : () -> ( Model, Cmd Msg )
init flags =
    ( Model "" Nothing [] "Loading orders...", loadOrders Load )



-- UPDATE


type Msg
    = RequestAdd
    | ConfirmAdd (Result Http.Error Int)
    | Load (Result Http.Error (List Model.Order))
    | UpdateName String
    | ToggleOrderType OrderType


validateNewOrder : String -> Maybe OrderType -> Result () ( String, OrderType )
validateNewOrder name orderType =
    case ( name, orderType ) of
        ( "", _ ) ->
            Err ()

        ( _, Nothing ) ->
            Err ()

        ( name1, Just orderType1 ) ->
            Ok ( name1, orderType1 )


isOk : Result a b -> Bool
isOk result =
    case result of
        Ok _ ->
            True

        Err _ ->
            False


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
                    ( model, requestAddOrder ConfirmAdd newName newOrderType )

        ConfirmAdd (Ok addedOrderId) ->
            ( { model | statusMessage = "Added order: id = " ++ fromInt addedOrderId }, loadOrders Load )

        ConfirmAdd (Err addError) ->
            ( { model | statusMessage = "Add error" }, Cmd.none )

        Load (Ok loadedOrders) ->
            ( { model | orders = loadedOrders, statusMessage = "Loaded orders!" }, Cmd.none )

        Load (Err loadError) ->
            ( { model | statusMessage = "Load Error" }, Cmd.none )

        UpdateName updatedName ->
            ( { model | newName = updatedName }, Cmd.none )

        ToggleOrderType newOrderType ->
            case Just newOrderType == model.newOrderType of
                True ->
                    ( { model | newOrderType = Nothing }, Cmd.none )

                False ->
                    ( { model | newOrderType = Just newOrderType }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text (fromInt (length model.orders)) ]
        , ul [] (List.map (\o -> li [] [ text (o.name ++ " " ++ orderTypeToString o.orderType) ]) (filterOrders model.orders model.newOrderType))
        , input [ type_ "text", placeholder "Name", value model.newName, onInput UpdateName ] []
        , orderButton model.newOrderType Eggs
        , orderButton model.newOrderType Honey
        , button [ onClick RequestAdd, disabled (not <| canAddOrder model.newName model.newOrderType) ] [ text "Add" ]
        , div [] [ text model.statusMessage ]
        ]


filterOrders : List Model.Order -> Maybe OrderType -> List Model.Order
filterOrders allOrders maybeSelectedOrderType =
    case maybeSelectedOrderType of
        Nothing ->
            allOrders

        Just selectedOrderType ->
            allOrders |> filter (\o -> o.orderType == selectedOrderType)


orderButton : Maybe OrderType -> OrderType -> Html Msg
orderButton existingOrderType orderType =
    button
        (concat
            [ [ onClick (ToggleOrderType orderType) ]
            , orderButtonStyle existingOrderType (Just orderType)
            ]
        )
        [ text (orderTypeToString orderType)
        ]


orderButtonStyle : a -> a -> List (Attribute Msg)
orderButtonStyle currentOrderType newOrderType =
    if currentOrderType == newOrderType then
        [ style "border-color" "red"
        , style "border-width" "2px"
        ]

    else
        []
