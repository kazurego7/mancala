module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, div)
import Set exposing (Set)
import Task
import Time


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias GameInitInfo =
    { playerIDs : Set PlayerID
    , pitNum : Int
    , pitInitSeedNum : Int
    , storeInitSeedNum : Int
    }


type alias PlayerID =
    String


type alias PlayerInfo =
    { pit : List Int
    , store : Int
    }


type alias GamePlayInfo =
    { playerInfos : Dict PlayerID PlayerInfo
    , turnOrder : List PlayerID
    , turnCount : Int
    , turnStartTime : Time.Posix
    }


type alias Model =
    GamePlayInfo


init : () -> ( Model, Cmd Msg )
init () =
    ( { playerIDs = Set.empty |> Set.insert "hoge" |> Set.insert "fuga"
      , pitNum = 6
      , pitInitSeedNum = 4
      , storeInitSeedNum = 0
      }
        |> initGameInfo
    , Task.perform GameStartTime Time.now
    )



-- UPDATE


type Msg
    = GameStartTime Time.Posix
    | NoMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GameStartTime posix ->
            ( { model | turnStartTime = posix }, Cmd.none )

        NoMsg ->
            ( model, Cmd.none )


initGameInfo : GameInitInfo -> GamePlayInfo
initGameInfo gameInitInfo =
    let
        playerInitInfo : PlayerInfo
        playerInitInfo =
            { pit =
                List.repeat gameInitInfo.pitNum 0
                    |> List.map (always gameInitInfo.pitInitSeedNum)
            , store =
                gameInitInfo.storeInitSeedNum
            }

        gamePlayInfo : GamePlayInfo
        gamePlayInfo =
            { playerInfos =
                gameInitInfo.playerIDs
                    |> Set.toList
                    |> List.map (\playerID -> ( playerID, playerInitInfo ))
                    |> Dict.fromList
            , turnOrder = gameInitInfo.playerIDs |> Set.toList
            , turnCount = 1
            , turnStartTime = Time.millisToPosix 0
            }
    in
    gamePlayInfo



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [] []
