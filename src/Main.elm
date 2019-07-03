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
    { playerID : PlayerID
    , pitSeedCounts : Array Int
    , storeSeedCount : Int
    }


type alias PitNumber =
    Int


type alias HoldPit =
    Maybe
        { pitNumber : PitNumber
        , seedCount : Int
        }


type alias GamePlayInfo =
    { playerInfoTable : Dict PlayerID PlayerInfo
    , nextOrderIDTable : Dict PlayerID PlayerID
    , pitCount : Int
    , holdPit : HoldPit
    , turnPlayerID : PlayerID
    , turnCount : Int
    , turnStartTime : Time.Posix
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
        initPlayerInfo : PlayerID -> PlayerInfo
        initPlayerInfo playerID =
            { playerID = playerID
            , pitSeedCounts =
                List.repeat gameInitInfo.pitCount 0
                    |> List.map (always gameInitInfo.initSeedCount)
                    |> Array.fromList
            , storeSeedCount = 0
            }

        playerIDs =
            gameInitInfo.playerIDs |> Set.toList

        -- playerIDsを先頭を後ろにやって、1つずつずらしたもの
        nextOrderIDs =
            let
                head =
                    playerIDs |> List.head |> Maybe.map List.singleton |> Maybe.withDefault []

                tail =
                    playerIDs |> List.tail |> Maybe.withDefault []
            in
            tail ++ head

        gamePlayInfo : GamePlayInfo
        gamePlayInfo =
            { playerInfoTable =
                playerIDs
                    |> List.map (\playerID -> ( playerID, initPlayerInfo playerID ))
                    |> Dict.fromList
            , pitCount = gameInitInfo.pitCount
            , nextOrderIDTable =
                List.map2 (\playerID nextOrderID -> ( playerID, nextOrderID )) playerIDs nextOrderIDs
                    |> Dict.fromList
            , turnCount = 1
            , turnPlayerID = playerIDs |> List.head |> Maybe.withDefault "None"
            , turnStartTime = Time.millisToPosix 0
            , holdPit = Maybe.Nothing
            }
    in
    gamePlayInfo


selectHoldPit : PitNumber -> GamePlayInfo -> GamePlayInfo
selectHoldPit newSelectedPitNumber gamePlayInfo =
    case gamePlayInfo.holdPit of
        Nothing ->
            gamePlayInfo

        Just holdPit ->
            if newSelectedPitNumber == holdPit.pitNumber then
                -- なにもしない
                gamePlayInfo

            else
                -- 別のpitを選ぶ
                let
                    newSeedCount =
                        gamePlayInfo.playerInfoTable
                            |> Dict.get gamePlayInfo.turnPlayerID
                            |> Maybe.andThen
                                (\playerInfo ->
                                    playerInfo.pitSeedCounts
                                        |> Array.get newSelectedPitNumber
                                )
                            |> Maybe.withDefault 0

                    updatePit : PlayerInfo -> PlayerInfo
                    updatePit playerInfo =
                        { playerInfo
                            | pitSeedCounts =
                                playerInfo.pitSeedCounts
                                    |> Array.set holdPit.pitNumber 0
                                    |> Array.set newSelectedPitNumber newSeedCount
                        }

                    newHoldPit =
                        Just
                            { holdPit
                                | pitNumber = newSelectedPitNumber
                                , seedCount = newSeedCount
                            }
                in
                { gamePlayInfo
                    | holdPit = newHoldPit
                    , playerInfoTable =
                        gamePlayInfo.playerInfoTable
                            |> Dict.update gamePlayInfo.turnPlayerID (Maybe.map updatePit)
                }


type HoleNumber
    = Pit PitNumber
    | Store


type alias IsMultiLap =
    Bool


sowing : GamePlayInfo -> ( GamePlayInfo, IsMultiLap )
sowing gamePlayInfo =
    -- Seedを次のPitに半時計回りに配っていく、また最後に配る先がstoreであればもう一度
    let
        initHoleNumber =
            Pit (gamePlayInfo.holdPit |> Maybe.map .pitNumber |> Maybe.withDefault 0)

        sowingHelper : HoleNumber -> PlayerID -> GamePlayInfo -> ( GamePlayInfo, IsMultiLap )
        sowingHelper holeNumber sowedPlayerID prevGamePlayInfo =
            let
                nextOrderID =
                    gamePlayInfo.nextOrderIDTable
                        |> Dict.get sowedPlayerID
                        |> Maybe.withDefault "None"

                seedCount =
                    gamePlayInfo.holdPit
                        |> Maybe.map .seedCount
                        |> Maybe.withDefault -1
            in
            case holeNumber of
                -- 今storeなので、次は隣のプレイヤーのpitにsowingする
                Store ->
                    let
                        addStoreSeed : PlayerInfo -> PlayerInfo
                        addStoreSeed playerInfo =
                            { playerInfo | storeSeedCount = playerInfo.storeSeedCount + 1 }

                        nextGamePlayInfo =
                            { gamePlayInfo
                                | playerInfoTable =
                                    gamePlayInfo.playerInfoTable
                                        |> Dict.update sowedPlayerID (Maybe.map addStoreSeed)
                                , holdPit =
                                    gamePlayInfo.holdPit
                                        |> Maybe.map (\prevholdPit -> { prevholdPit | seedCount = prevholdPit.seedCount - 1 })
                            }
                    in
                    -- 最後のseedがStoreに配られるとき、multiLapになる
                    if seedCount == 1 then
                        ( nextGamePlayInfo, True )

                    else
                        sowingHelper (Pit 0) nextOrderID nextGamePlayInfo

                Pit pitNumber ->
                    let
                        isSowedToTurnPlayer =
                            sowedPlayerID == gamePlayInfo.turnPlayerID

                        isSowedToTurnPlayerPit =
                            gamePlayInfo.holdPit |> Maybe.map (\holdPit -> pitNumber == holdPit.pitNumber) |> Maybe.withDefault False

                        nextHoleNumber =
                            if pitNumber + 1 == gamePlayInfo.pitCount then
                                Store

                            else
                                Pit (pitNumber + 1)
                    in
                    -- 配る先が自分の取ったpitなら飛ばす
                    if isSowedToTurnPlayer && isSowedToTurnPlayerPit then
                        sowingHelper nextHoleNumber sowedPlayerID prevGamePlayInfo

                    else
                        let
                            addPitSeed : PlayerInfo -> PlayerInfo
                            addPitSeed playerInfo =
                                { playerInfo | pitSeedCounts = playerInfo.pitSeedCounts |> Array.Extra.update pitNumber (\pitCount -> pitCount + 1) }

                            nextGamePlayInfo =
                                { gamePlayInfo
                                    | playerInfoTable =
                                        gamePlayInfo.playerInfoTable
                                            |> Dict.update sowedPlayerID (Maybe.map addPitSeed)
                                    , holdPit =
                                        gamePlayInfo.holdPit
                                            |> Maybe.map (\prevholdPit -> { prevholdPit | seedCount = prevholdPit.seedCount - 1 })
                                }
                        in
                        if seedCount == 1 then
                            ( nextGamePlayInfo, False )

                        else
                            sowingHelper nextHoleNumber nextOrderID nextGamePlayInfo
    in
    sowingHelper initHoleNumber gamePlayInfo.turnPlayerID gamePlayInfo



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [] []
