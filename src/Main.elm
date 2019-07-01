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
    , pitCount : Int
    , initSeedCount : Int
    }


type alias PlayerID =
    String


type alias PlayerInfo =
    { pitSeedCounts : List Int
    , storeSeedCount : Int
    }


type alias PickedSeed =
    Maybe
        { pitNumber : Int
        , seedCount : Int
        }


type alias GamePlayInfo =
    { playerInfos : Dict PlayerID PlayerInfo
    , turnOrder : List PlayerID
    , turnCount : Int
    , turnStartTime : Time.Posix
    , pickedSeed : PickedSeed
    }


type alias Model =
    GamePlayInfo


init : () -> ( Model, Cmd Msg )
init () =
    ( { playerIDs = Set.empty |> Set.insert "hoge" |> Set.insert "fuga"
      , pitCount = 6
      , initSeedCount = 4
      }
        |> initGamePlayInfo
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


initGamePlayInfo : GameInitInfo -> GamePlayInfo
initGamePlayInfo gameInitInfo =
    let
        playerInitInfo : PlayerInfo
        playerInitInfo =
            { pitSeedCounts =
                List.repeat gameInitInfo.pitCount 0
                    |> List.map (always gameInitInfo.initSeedCount)
            , storeSeedCount = 0
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
            , pickedSeed = Maybe.Nothing
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
