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
    , nextOrderID : PlayerID
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
    , pitCount : Int
    , turnPlayerID : PlayerID
    , turnCount : Int
    , turnStartTime : Time.Posix
    , holdPit : HoldPit
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
        initPlayerInfo : PlayerID -> PlayerID -> PlayerInfo
        initPlayerInfo playerID nextOrderID =
            { playerID = playerID
            , pitSeedCounts =
                List.repeat gameInitInfo.pitCount 0
                    |> List.map (always gameInitInfo.initSeedCount)
                    |> Array.fromList
            , storeSeedCount = 0
            , nextOrderID = nextOrderID
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
                List.map2 (\playerID nextOrderID -> ( playerID, initPlayerInfo playerID nextOrderID )) playerIDs nextOrderIDs
                    |> Dict.fromList
            , pitCount = gameInitInfo.pitCount
            , turnPlayerID = playerIDs |> List.head |> Maybe.withDefault "None"
            , turnCount = 1
            , turnStartTime = Time.millisToPosix 0
            , holdPit = Maybe.Nothing
            }
    in
    gamePlayInfo


selectSowingPit : PitNumber -> GamePlayInfo -> GamePlayInfo
selectSowingPit newSelectedPitNumber gamePlayInfo =
    case gamePlayInfo.holdPit of
        Nothing ->
            gamePlayInfo

        Just holdPit ->
            if newSelectedPitNumber == holdPit.pitNumber then
                -- なにもしない
                gamePlayInfo

            else
                let
                    seedCount =
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
                                    |> Array.set newSelectedPitNumber seedCount
                        }
                in
                -- 別のpitを選ぶ
                { gamePlayInfo
                    | holdPit =
                        Just
                            { holdPit
                                | pitNumber = newSelectedPitNumber
                                , seedCount = seedCount
                            }
                    , playerInfoTable =
                        gamePlayInfo.playerInfoTable
                            |> Dict.update gamePlayInfo.turnPlayerID (Maybe.map updatePit)
                }


type HoleNumber
    = Pit PitNumber
    | Store


type alias IsMultiLap =
    Bool


updatePlayerInfo : PlayerID -> (PlayerInfo -> PlayerInfo) -> GamePlayInfo -> GamePlayInfo
updatePlayerInfo playerID updateProcess gamePlayInfo =
    { gamePlayInfo
        | playerInfoTable =
            gamePlayInfo.playerInfoTable
                |> Dict.update playerID (Maybe.map updateProcess)
    }


sowing : GamePlayInfo -> ( GamePlayInfo, IsMultiLap )
sowing gamePlayInfo =
    -- Seedを次のPitに半時計回りに配っていく、また最後に配る先がstoreであればもう一度
    let
        initHoleNumber =
            Pit (gamePlayInfo.holdPit |> Maybe.map .pitNumber |> Maybe.withDefault 0)

        initSeedCount =
            gamePlayInfo.holdPit |> Maybe.map .seedCount |> Maybe.withDefault 0

        sowingHelper : HoleNumber -> Int -> PlayerID -> GamePlayInfo -> ( GamePlayInfo, IsMultiLap )
        sowingHelper holeNumber seedCount sowedPlayerID prevGamePlayInfo =
            let
                nextOrderID =
                    gamePlayInfo.playerInfoTable
                        |> Dict.get sowedPlayerID
                        |> Maybe.map .nextOrderID
                        |> Maybe.withDefault "None"
            in
            case holeNumber of
                -- 今storeなので、次は隣のプレイヤーのpitにsowingする
                Store ->
                    let
                        addStoreSeed : PlayerInfo -> PlayerInfo
                        addStoreSeed playerInfo =
                            { playerInfo | storeSeedCount = playerInfo.storeSeedCount + 1 }

                        nextGamePlayInfo =
                            updatePlayerInfo prevGamePlayInfo.turnPlayerID addStoreSeed prevGamePlayInfo
                    in
                    -- 最後のseedがStoreに配られるとき、multiLapになる
                    if seedCount == 1 then
                        ( nextGamePlayInfo, True )

                    else
                        sowingHelper (Pit 0) (seedCount - 1) nextOrderID nextGamePlayInfo

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
                        sowingHelper nextHoleNumber seedCount sowedPlayerID prevGamePlayInfo

                    else
                        let
                            addPitSeed : PlayerInfo -> PlayerInfo
                            addPitSeed playerInfo =
                                { playerInfo | pitSeedCounts = playerInfo.pitSeedCounts |> Array.Extra.update pitNumber (\pitCount -> pitCount + 1) }

                            nextGamePlayInfo =
                                updatePlayerInfo prevGamePlayInfo.turnPlayerID addPitSeed prevGamePlayInfo
                        in
                        if seedCount == 1 then
                            ( nextGamePlayInfo, False )

                        else
                            sowingHelper nextHoleNumber (seedCount - 1) nextOrderID nextGamePlayInfo
    in
    sowingHelper initHoleNumber initSeedCount gamePlayInfo.turnPlayerID gamePlayInfo



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [] []
