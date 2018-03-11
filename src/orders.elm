import Html exposing (..)
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
  , order: String
  }

type alias Order =
  { id: Int
  , name: String
  , order: String
  , datePlaced: String}

type alias Model =
  { newOrder : NewOrder
  , orders : List Order 
  }


init : (Model, Cmd Msg)
init =
  (Model { name = "TestName", order = "" } [], Cmd.none)



-- UPDATE


type Msg
  = Add NewOrder
  | Remove Int


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Add orderToAdd ->
      (Model model.newOrder ({ id = 1, name = orderToAdd.name, order = orderToAdd.order, datePlaced = "2018-01-01" } :: model.orders), Cmd.none)

    Remove orderId ->
        (model, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text (model.newOrder.name) ]
    , h2 [] [ text (toString (List.length model.orders))]
    , button [ onClick (Add { name = "New", order = "Order" }) ] [ text "Add" ]
    ]
