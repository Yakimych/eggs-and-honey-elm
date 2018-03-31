module Orders exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Result.Extra exposing (isOk)


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


orderToString : OrderType -> String
orderToString order =
    case order of
        Eggs ->
            "Eggs"

        Honey ->
            "Honey"


type alias Order =
    { id : Int
    , name : String
    , order : OrderType
    , datePlaced : String
    }


type alias Model =
    { newName : String
    , newOrder : Maybe OrderType
    , orders : List Order
    }


init : ( Model, Cmd Msg )
init =
    ( Model "" Nothing [], Cmd.none )



-- UPDATE


type Msg
    = Add
    | UpdateName String
    | ToggleOrderType OrderType


validateNewOrder : String -> Maybe OrderType -> Result () ( String, OrderType )
validateNewOrder name order =
    case ( name, order ) of
        ( "", _ ) ->
            Err ()

        ( _, Nothing ) ->
            Err ()

        ( name, Just order ) ->
            Ok ( name, order )


canAddOrder : String -> Maybe OrderType -> Bool
canAddOrder name order =
    validateNewOrder name order |> isOk


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Add ->
            case validateNewOrder model.newName model.newOrder of
                Err () ->
                    ( model, Cmd.none )

                Ok ( newName, newOrder ) ->
                    ( Model "" model.newOrder ({ id = 1, name = model.newName, order = newOrder, datePlaced = "2018-01-01" } :: model.orders), Cmd.none )

        UpdateName updatedName ->
            ( { model | newName = updatedName }, Cmd.none )

        ToggleOrderType newOrderType ->
            case (Just newOrderType) == model.newOrder of
                True ->
                    ( { model | newOrder = Nothing }, Cmd.none )

                False ->
                    ( { model | newOrder = Just newOrderType }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text (toString (List.length model.orders)) ]
        , ul [] (List.map (\o -> li [] [ text (o.name ++ " " ++ (orderToString o.order)) ]) model.orders)
        , input [ type_ "text", placeholder "Name", value model.newName, onInput UpdateName ] []
        , orderButton model.newOrder Eggs
        , orderButton model.newOrder Honey
        , button [ onClick Add, disabled (not <| canAddOrder model.newName model.newOrder) ] [ text "Add" ]
        ]


orderButton : Maybe OrderType -> OrderType -> Html Msg
orderButton existingOrderType orderType =
    button
        [ onClick (ToggleOrderType orderType)
        , style (orderButtonStyle existingOrderType (Just orderType))
        ]
        [ text (orderToString orderType)
        ]


orderButtonStyle : a -> a -> List ( String, String )
orderButtonStyle currentOrderType newOrderType =
    if currentOrderType == newOrderType then
        [ ( "border-color", "red" )
        , ( "border-width", "2px" )
        ]
    else
        []
