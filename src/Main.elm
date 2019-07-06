module Main exposing (Model, Msg(..), init, main, update, view)

import Array exposing (Array)
import Array.Extra
import Browser
import Dict exposing (Dict)
import Html exposing (Html, button, div, text)
import List.Extra
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


type alias PlayerID =
    String


defaultPlayerID =
    "NoID"


type alias GameInitInfo =
    { playerIDs : Set PlayerID
    , pitCount : Int
    , initSeedCount : Int
    , timeLimitForSecond : Int
    }


type alias PlayerInfo =
    { playerID : PlayerID
    , pitSeedCounts : Array Int
    , storeSeedCount : Int
    }


defaultPlayerInfo : PlayerInfo
defaultPlayerInfo =
    { playerID = defaultPlayerID
    , pitSeedCounts = Array.empty
    , storeSeedCount = -1
    }


type alias PitNumber =
    Int


type HoleNumber
    = Pit PitNumber
    | Store


type alias HoldPit =
    Maybe
        { sowingPitNumber : PitNumber
        , seedCount : Int
        }


type alias SowedHole =
    Maybe
        { sowedPlayerID : PlayerID
        , sowedHoleNumber : HoleNumber
        }


type alias GamePlayInfo =
    { playerInfoTable : Dict PlayerID PlayerInfo
    , nextOrderIDTable : Dict PlayerID PlayerID
    , pitCount : Int
    , holdPit : HoldPit
    , sowedHole : SowedHole
    , myID : PlayerID
    , turnPlayerID : PlayerID
    , turnCount : Int
    , turnStartTime : Time.Posix
    , timeLimitForSecond : Int
    }


getPlayerInfo : PlayerID -> GamePlayInfo -> PlayerInfo
getPlayerInfo playerID gamePlayInfo =
    gamePlayInfo.playerInfoTable
        |> Dict.get playerID
        |> Maybe.withDefault defaultPlayerInfo


getNextOrderID : PlayerID -> GamePlayInfo -> PlayerID
getNextOrderID playerID gamePlayInfo =
    gamePlayInfo.nextOrderIDTable
        |> Dict.get playerID
        |> Maybe.withDefault defaultPlayerID


getOrderedIDs : GamePlayInfo -> List PlayerID
getOrderedIDs gamePlayInfo =
    let
        playerCount =
            gamePlayInfo.playerInfoTable
                |> Dict.size
    in
    List.repeat (playerCount - 1) ()
        |> List.Extra.scanl (\_ prevPlayerID -> getNextOrderID prevPlayerID gamePlayInfo) gamePlayInfo.myID


type alias Model =
    GamePlayInfo


init : () -> ( Model, Cmd Msg )
init () =
    ( { playerIDs = Set.empty |> Set.insert "hoge" |> Set.insert "fuga"
      , pitCount = 6
      , initSeedCount = 4
      , timeLimitForSecond = 30
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
    in
    { playerInfoTable =
        playerIDs
            |> List.map (\playerID -> ( playerID, initPlayerInfo playerID ))
            |> Dict.fromList
    , pitCount = gameInitInfo.pitCount
    , nextOrderIDTable =
        List.map2 (\playerID nextOrderID -> ( playerID, nextOrderID )) playerIDs nextOrderIDs
            |> Dict.fromList
    , turnCount = 1
    , myID = "hoge"
    , turnPlayerID = playerIDs |> List.head |> Maybe.withDefault defaultPlayerID
    , turnStartTime = Time.millisToPosix 0
    , holdPit = Nothing
    , sowedHole = Nothing
    , timeLimitForSecond = gameInitInfo.timeLimitForSecond
    }


selectHoldPit : PitNumber -> GamePlayInfo -> GamePlayInfo
selectHoldPit newSelectedPitNumber gamePlayInfo =
    let
        newHoldPit =
            Just
                { sowingPitNumber = newSelectedPitNumber
                , seedCount =
                    getPlayerInfo gamePlayInfo.turnPlayerID gamePlayInfo
                        |> .pitSeedCounts
                        |> Array.get newSelectedPitNumber
                        |> Maybe.withDefault -1
                }
    in
    case gamePlayInfo.holdPit of
        Nothing ->
            --
            let
                updatePit : PlayerInfo -> PlayerInfo
                updatePit playerInfo =
                    { playerInfo
                        | pitSeedCounts =
                            playerInfo.pitSeedCounts
                                -- 新しいpitからseedを取る処理
                                |> Array.set newSelectedPitNumber 0
                    }
            in
            { gamePlayInfo
                | holdPit = newHoldPit
                , playerInfoTable =
                    gamePlayInfo.playerInfoTable
                        |> Dict.update gamePlayInfo.turnPlayerID (Maybe.map updatePit)
            }

        Just holdPit ->
            if newSelectedPitNumber == holdPit.sowingPitNumber then
                -- なにもしない
                gamePlayInfo

            else
                -- 現在選んでいるpitへseedを戻し、別のpitを選んでseedをとる
                let
                    updatePit : PlayerInfo -> PlayerInfo
                    updatePit playerInfo =
                        { playerInfo
                            | pitSeedCounts =
                                playerInfo.pitSeedCounts
                                    --元のpitへseedを戻す処理
                                    |> Array.set holdPit.sowingPitNumber holdPit.seedCount
                                    -- 新しいpitからseedを取る処理
                                    |> Array.set newSelectedPitNumber 0
                        }
                in
                { gamePlayInfo
                    | holdPit = newHoldPit
                    , playerInfoTable =
                        gamePlayInfo.playerInfoTable
                            |> Dict.update gamePlayInfo.turnPlayerID (Maybe.map updatePit)
                }


sowing : GamePlayInfo -> GamePlayInfo
sowing gamePlayInfo =
    case ( gamePlayInfo.holdPit, gamePlayInfo.sowedHole ) of
        ( Just prevHoldPit, Nothing ) ->
            let
                -- pitの次は、必ず自分のpitかstore
                nextHoleNumber =
                    if prevHoldPit.sowingPitNumber + 1 == gamePlayInfo.pitCount then
                        Store

                    else
                        Pit (prevHoldPit.sowingPitNumber + 1)
            in
            { gamePlayInfo
                | sowedHole = Just { sowedPlayerID = gamePlayInfo.turnPlayerID, sowedHoleNumber = nextHoleNumber }
            }

        ( Just prevHoldPit, Just prevSowedHole ) ->
            let
                nextOrderID =
                    gamePlayInfo.nextOrderIDTable
                        |> Dict.get prevSowedHole.sowedPlayerID
                        |> Maybe.withDefault defaultPlayerID
            in
            case prevSowedHole.sowedHoleNumber of
                Store ->
                    -- 今storeなので、次は隣のプレイヤーのpitにsowingする
                    let
                        addStoreSeed : PlayerInfo -> PlayerInfo
                        addStoreSeed playerInfo =
                            { playerInfo | storeSeedCount = playerInfo.storeSeedCount + 1 }
                    in
                    { gamePlayInfo
                        | playerInfoTable =
                            gamePlayInfo.playerInfoTable
                                |> Dict.update prevSowedHole.sowedPlayerID (Maybe.map addStoreSeed)
                        , holdPit = Just { prevHoldPit | seedCount = prevHoldPit.seedCount - 1 }
                        , sowedHole = Just { sowedPlayerID = nextOrderID, sowedHoleNumber = Pit 0 }
                    }

                Pit pitNumber ->
                    let
                        isSowedToTurnPlayer =
                            prevSowedHole.sowedPlayerID == gamePlayInfo.turnPlayerID

                        isSowedToTurnPlayerPit =
                            pitNumber == prevHoldPit.sowingPitNumber

                        -- pitの次は、必ず自分のpitかstore
                        nextHoleNumber =
                            if pitNumber + 1 == gamePlayInfo.pitCount then
                                Store

                            else
                                Pit (pitNumber + 1)
                    in
                    if isSowedToTurnPlayer && isSowedToTurnPlayerPit then
                        -- 配る先が自分の取ったpitなら飛ばす
                        { gamePlayInfo
                            | sowedHole = Just { sowedPlayerID = nextOrderID, sowedHoleNumber = nextHoleNumber }
                        }

                    else
                        -- 配る先がpitなら普通に配る
                        let
                            addPitSeed : PlayerInfo -> PlayerInfo
                            addPitSeed playerInfo =
                                { playerInfo | pitSeedCounts = playerInfo.pitSeedCounts |> Array.Extra.update pitNumber (\pitCount -> pitCount + 1) }
                        in
                        { gamePlayInfo
                            | playerInfoTable =
                                gamePlayInfo.playerInfoTable
                                    |> Dict.update prevSowedHole.sowedPlayerID (Maybe.map addPitSeed)
                            , holdPit = Just { prevHoldPit | seedCount = prevHoldPit.seedCount - 1 }
                            , sowedHole = Just { sowedPlayerID = nextOrderID, sowedHoleNumber = nextHoleNumber }
                        }

        _ ->
            gamePlayInfo



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


viewBoardInfo : GamePlayInfo -> Html Msg
viewBoardInfo gamePlayInfo =
    let
        viewPlayer : PlayerInfo -> Html Msg
        viewPlayer playerInfo =
            let
                storeText =
                    text (String.fromInt playerInfo.storeSeedCount)
            in
            if playerInfo.playerID == gamePlayInfo.turnPlayerID then
                let
                    pitButtons =
                        playerInfo.pitSeedCounts
                            |> Array.map String.fromInt
                            |> Array.map (\seedCount -> button [] [ text seedCount ])
                            |> Array.toList
                in
                div [] (pitButtons ++ [ storeText ])

            else
                let
                    pitTexts =
                        playerInfo.pitSeedCounts
                            |> Array.map String.fromInt
                            |> Array.map text
                            |> Array.toList
                in
                div [] (pitTexts ++ [ storeText ])

        turnPlayerInfo =
            getPlayerInfo gamePlayInfo.turnPlayerID gamePlayInfo

        otherPlayerInfos =
            gamePlayInfo.playerInfoTable
                |> Dict.remove gamePlayInfo.turnPlayerID
                |> Dict.values
    in
    getOrderedIDs gamePlayInfo
        |> List.map (\orderedID -> getPlayerInfo orderedID gamePlayInfo)
        |> List.map viewPlayer
        |> div []


view : Model -> Html Msg
view model =
    div [] [ viewBoardInfo model ]
