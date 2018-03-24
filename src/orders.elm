module Main exposing (..)

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


type alias Order =
    { id : Int
    , name : String
    , order : String
    , datePlaced : String
    }


type alias Model =
    { newName : String
    , newOrder : String
    , orders : List Order
    }


init : ( Model, Cmd Msg )
init =
    ( Model "" "" [], Cmd.none )



-- UPDATE


type Msg
    = Add
    | Remove Int
    | AddedName String
    | AddedOrder String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Add ->
            ( Model "" model.newOrder ({ id = 1, name = model.newName, order = model.newOrder, datePlaced = "2018-01-01" } :: model.orders), Cmd.none )

        Remove orderId ->
            ( model, Cmd.none )

        AddedName addedName ->
            ( { model | newName = addedName }, Cmd.none )

        AddedOrder addedOrder ->
            ( { model | newOrder = addedOrder }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text (model.newOrder) ]
        , h2 [] [ text (toString (List.length model.orders)) ]
        , ul [] (List.map (\o -> li [] [ text (o.name ++ " " ++ o.order) ]) model.orders)
        , input [ type_ "text", placeholder "Name", value model.newName, onInput AddedName ] []
        , input [ type_ "text", placeholder "Order", onInput AddedOrder ] []
        , button [ onClick Add ] [ text "Add" ]
        ]
