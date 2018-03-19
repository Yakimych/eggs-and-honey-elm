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


type alias NewOrder =
    { name : String
    , order : String
    }


type alias Order =
    { id : Int
    , name : String
    , order : String
    , datePlaced : String
    }


type alias Model =
    { newOrder : NewOrder
    , orders : List Order
    }


init : ( Model, Cmd Msg )
init =
    ( Model { name = "", order = "" } [], Cmd.none )



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
            ( Model { name = model.newOrder.name, order = model.newOrder.order } ({ id = 1, name = model.newOrder.name, order = model.newOrder.order, datePlaced = "2018-01-01" } :: model.orders), Cmd.none )

        Remove orderId ->
            ( model, Cmd.none )

        -- TODO: Do this is a prettier (more concise) way (without duplication)?
        AddedName addedName ->
            let
                updatedModel =
                    model.newOrder
            in
                ( { model | newOrder = { updatedModel | name = addedName } }, Cmd.none )

        AddedOrder addedOrder ->
            let
                updatedModel =
                    model.newOrder
            in
                ( { model | newOrder = { updatedModel | order = addedOrder } }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text (model.newOrder.order) ]
        , h2 [] [ text (toString (List.length model.orders)) ]
        , ul [] (List.map (\o -> li [] [ text (o.name ++ " " ++ o.order) ]) model.orders)
        , input [ type_ "text", placeholder "Name", onInput AddedName ] []
        , input [ type_ "text", placeholder "Order", onInput AddedOrder ] []
        , button [ onClick Add ] [ text "Add" ]
        ]
