module Orders exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List exposing (length, append, filter)
import Result.Extra exposing (isOk)
import Http
import Model exposing (orderTypeToString, OrderType(..))
import DataProvider exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { newName : String
    , newOrderType : Maybe OrderType
    , orders : List Model.Order
    , statusMessage : String
    }


init : ( Model, Cmd Msg )
init =
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
                    ( model, requestAddOrder ConfirmAdd newName newOrderType )

        ConfirmAdd (Ok addedOrderId) ->
            ( { model | statusMessage = ("Added order: id = " ++ (toString addedOrderId)) }, loadOrders Load )

        ConfirmAdd (Err addError) ->
            ( { model | statusMessage = toString addError }, Cmd.none )

        Load (Ok loadedOrders) ->
            ( { model | orders = loadedOrders, statusMessage = "Loaded orders!" }, Cmd.none )

        Load (Err loadError) ->
            ( { model | statusMessage = toString loadError }, Cmd.none )

        UpdateName updatedName ->
            ( { model | newName = updatedName }, Cmd.none )

        ToggleOrderType newOrderType ->
            case (Just newOrderType) == model.newOrderType of
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
        [ h2 [] [ text (toString (length model.orders)) ]
        , ul [] (List.map (\o -> li [] [ text (o.name ++ " " ++ (orderTypeToString o.orderType)) ]) (filterOrders model.orders model.newOrderType))
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
