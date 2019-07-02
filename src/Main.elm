module Main exposing (Model, Msg(..), init, main, update, view)

import Array exposing (Array)
import Array.Extra
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
    { pitSeedCounts : Array Int
    , storeSeedCount : Int
    }


type alias PitNumber =
    Int


type alias PickedSeed =
    Maybe
        { pitNumber : PitNumber
        , seedCount : Int
        }


type alias GamePlayInfo =
    { playerInfoTable : Dict PlayerID PlayerInfo
    , turnOrder : Array PlayerID
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
                    |> Array.fromList
            , storeSeedCount = 0
            }

        gamePlayInfo : GamePlayInfo
        gamePlayInfo =
            { playerInfoTable =
                gameInitInfo.playerIDs
                    |> Set.toList
                    |> List.map (\playerID -> ( playerID, playerInitInfo ))
                    |> Dict.fromList
            , turnOrder =
                gameInitInfo.playerIDs
                    |> Set.toList
                    |> Array.fromList
            , turnCount = 1
            , turnStartTime = Time.millisToPosix 0
            , pickedSeed = Maybe.Nothing
            }
    in
    gamePlayInfo


pickSeed : PitNumber -> GamePlayInfo -> GamePlayInfo
pickSeed newPitNumber gamePlayInfo =
    case gamePlayInfo.pickedSeed of
        Nothing ->
            gamePlayInfo

        Just pickedSeed ->
            let
                turnPlayerID : Maybe PlayerID
                turnPlayerID =
                    getTurnPlayerID gamePlayInfo

                -- newPitNumberのseedが0のとき、つまめない (count=0)
                pickCount : Int
                pickCount =
                    turnPlayerID
                        |> Maybe.andThen (\key -> Dict.get key gamePlayInfo.playerInfoTable)
                        |> Maybe.andThen (\playerInfo -> Array.get newPitNumber playerInfo.pitSeedCounts)
                        |> Maybe.map (\n -> n == 0)
                        |> Maybe.map
                            (\canPick ->
                                if canPick then
                                    1

                                else
                                    0
                            )
                        |> Maybe.withDefault 0
            in
            if newPitNumber == pickedSeed.pitNumber then
                -- 同じpitのseedをもう1つつまむ
                let
                    pickAnotherOneSeed : PlayerInfo -> PlayerInfo
                    pickAnotherOneSeed playerInfo =
                        { playerInfo
                            | pitSeedCounts =
                                playerInfo.pitSeedCounts
                                    |> Array.Extra.update pickedSeed.pitNumber (\n -> n - pickCount)
                        }
                in
                { gamePlayInfo
                    | playerInfoTable =
                        gamePlayInfo.playerInfoTable
                            |> Dict.update (turnPlayerID |> Maybe.withDefault "None") (Maybe.map pickAnotherOneSeed)
                    , pickedSeed = Just { pickedSeed | seedCount = pickedSeed.seedCount + pickCount }
                }

            else
                -- つまんでいたseedを元のpitに戻し、新しく選んだpitのseedを1つつまむ
                let
                    pickOtherSeed : PlayerInfo -> PlayerInfo
                    pickOtherSeed playerInfo =
                        { playerInfo
                            | pitSeedCounts =
                                playerInfo.pitSeedCounts
                                    |> Array.Extra.update pickedSeed.pitNumber (\seedCount -> seedCount + pickedSeed.seedCount)
                                    |> Array.Extra.update newPitNumber (\seedCount -> seedCount - pickCount)
                        }
                in
                { gamePlayInfo
                    | playerInfoTable =
                        gamePlayInfo.playerInfoTable
                            |> Dict.update (turnPlayerID |> Maybe.withDefault "None") (Maybe.map pickOtherSeed)
                    , pickedSeed = Just { pickedSeed | pitNumber = newPitNumber, seedCount = 1 }
                }


getTurnPlayerID : GamePlayInfo -> Maybe PlayerID
getTurnPlayerID gamePlayInfo =
    let
        playerCount =
            gamePlayInfo.playerInfoTable |> Dict.size

        orderIndex =
            modBy gamePlayInfo.turnCount playerCount
    in
    gamePlayInfo.turnOrder |> Array.get orderIndex



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [] []
