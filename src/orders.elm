module Orders exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


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


maybeOrderFromString : String -> Maybe OrderType
maybeOrderFromString orderString =
    case orderString of
        "Eggs" ->
            Just Eggs

        "Honey" ->
            Just Honey

        _ ->
            Nothing


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
    | Remove Int
    | AddedName String
    | AddedOrder String


type Either a b
    = Left a
    | Right b


toBool : Either a b -> Bool
toBool eitherValue =
    case eitherValue of
        Left _ ->
            True

        Right _ ->
            False


validateNewOrder : String -> Maybe a -> Either ( String, a ) ()
validateNewOrder name order =
    case ( name, order ) of
        ( "", _ ) ->
            Right ()

        ( _, Nothing ) ->
            Right ()

        ( name, Just order ) ->
            Left ( name, order )


canAddOrder : String -> Maybe a -> Bool
canAddOrder name order =
    validateNewOrder name order |> toBool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Add ->
            case validateNewOrder model.newName model.newOrder of
                Right () ->
                    ( model, Cmd.none )

                Left ( newName, newOrder ) ->
                    ( Model "" model.newOrder ({ id = 1, name = model.newName, order = newOrder, datePlaced = "2018-01-01" } :: model.orders), Cmd.none )

        Remove orderId ->
            ( model, Cmd.none )

        AddedName addedName ->
            ( { model | newName = addedName }, Cmd.none )

        AddedOrder addedOrder ->
            ( { model | newOrder = maybeOrderFromString addedOrder }, Cmd.none )



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
        , input [ type_ "text", placeholder "Name", value model.newName, onInput AddedName ] []
        , input [ type_ "text", placeholder "Order", onInput AddedOrder ] []
        , button [ onClick Add, disabled (not <| canAddOrder model.newName model.newOrder) ] [ text "Add" ]
        ]
