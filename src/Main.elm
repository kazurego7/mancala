module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, div)
import Time exposing (Posix)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias UserID =
    Int


type alias SeedNum =
    Int


type alias TurnInfo =
    { turnStartTime : Posix
    , elapsedTime : Int
    }


type alias PlayerInfo =
    { pit : List SeedNum
    , store : SeedNum
    , turnInfo : Maybe TurnInfo
    }


type alias GameInfo =
    { playersID : List UserID
    , playersInfo : Dict UserID PlayerInfo
    }


type alias Model =
    Int


init : Model
init =
    0



-- UPDATE


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1



-- VIEW


view : Model -> Html Msg
view model =
    div [] []
