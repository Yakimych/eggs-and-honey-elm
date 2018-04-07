module Model exposing (OrderType(..), Order, orderTypeToString)


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
