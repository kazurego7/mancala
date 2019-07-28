module Main exposing (Model, Msg(..), init, main, update, view)

import Array exposing (Array)
import Array.Extra
import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra
import Random
import Random.List
import Set exposing (Set)
import String.Extra
import Task
import Time
import Time.Extra


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


defaultPlayerID : PlayerID
defaultPlayerID =
    "NoID"


type alias GameInitInfo =
    { playerIDs : List PlayerID
    , entryPlayerName : String
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


type alias HoldPitInfo =
    { pitNumber : PitNumber
    , seedCount : Int
    }


type alias SowedInfo =
    { playerID : PlayerID
    , holeNumber : HoleNumber
    }


type alias PlayerInfoTable =
    Dict PlayerID PlayerInfo


type alias NextOrderIDTable =
    Dict PlayerID PlayerID


type alias GamePlayInfo =
    { playerInfoTable : PlayerInfoTable
    , nextOrderIDTable : NextOrderIDTable
    , pitCount : Int
    , initSeedCount : Int
    , cliantID : PlayerID
    , turnPlayerID : PlayerID
    , turnCount : Int
    , nowTime : Time.Posix
    , turnStartTime : Time.Posix
    , timeLimitForSecond : Int
    , holdPit : Maybe HoldPitInfo
    , sowedHole : Maybe SowedInfo
    }


type alias GameEndInfo =
    { playerInfoTable : PlayerInfoTable
    , nextOrderIDTable : NextOrderIDTable
    , pitCount : Int
    , initSeedCount : Int
    , cliantID : PlayerID
    , turnCount : Int
    , reminingTime : Int
    , timeLimitForSecond : Int
    , winnerID : PlayerID
    }


getPlayerInfo : PlayerID -> PlayerInfoTable -> PlayerInfo
getPlayerInfo playerID playerInfoTable =
    playerInfoTable
        |> Dict.get playerID
        |> Maybe.withDefault defaultPlayerInfo


getNextOrderID : PlayerID -> NextOrderIDTable -> PlayerID
getNextOrderID playerID nextOrderIDTable =
    nextOrderIDTable
        |> Dict.get playerID
        |> Maybe.withDefault defaultPlayerID


getOrderedIDs : PlayerID -> NextOrderIDTable -> List PlayerID
getOrderedIDs cliantID nextOrderIDTable =
    let
        playerCount =
            nextOrderIDTable
                |> Dict.size
    in
    List.repeat (playerCount - 1) ()
        |> List.Extra.scanl (\_ prevPlayerID -> getNextOrderID prevPlayerID nextOrderIDTable) cliantID


getReminingTime : Time.Posix -> Time.Posix -> Int -> Int
getReminingTime turnStartTime nowTime timeLimitForSecond =
    let
        elapsedSecond =
            Time.Extra.diff Time.Extra.Second Time.utc turnStartTime nowTime
    in
    timeLimitForSecond - elapsedSecond


getPlayerIsWin : PlayerInfo -> Bool
getPlayerIsWin playerInfo =
    playerInfo
        |> .pitSeedCounts
        |> Array.toList
        |> List.all (\seedCount -> seedCount == 0)


type Model
    = GameInit GameInitInfo
    | GamePlay GamePlayInfo
    | GameEnd GameEndInfo


dammyGameInitInfo : GameInitInfo
dammyGameInitInfo =
    { playerIDs = []
    , entryPlayerName = ""
    , pitCount = 6
    , initSeedCount = 4
    , timeLimitForSecond = 40
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( GameInit dammyGameInitInfo, Cmd.none )



-- UPDATE


type Msg
    = Tick Time.Posix
    | SelectPitCount Int
    | SelectSeedCount Int
    | SelectTimeLimit Int
    | InputPlayerName String
    | EntryPlayer PlayerID
    | ExitPlayer PlayerID
    | StartGame
    | GetTurnStartTime Time.Posix
    | InitOrderIDs (List PlayerID)
    | SelectHoldPit PitNumber
    | Sowing
    | RestartGame
    | InitGame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( GameInit gameInitInfo, SelectPitCount n ) ->
            ( { gameInitInfo | pitCount = n } |> GameInit, Cmd.none )

        ( GameInit gameInitInfo, SelectSeedCount n ) ->
            ( { gameInitInfo | initSeedCount = n } |> GameInit, Cmd.none )

        ( GameInit gameInitInfo, SelectTimeLimit t ) ->
            ( { gameInitInfo | timeLimitForSecond = t } |> GameInit, Cmd.none )

        ( GameInit gameInitInfo, InputPlayerName playerName ) ->
            ( { gameInitInfo | entryPlayerName = playerName } |> GameInit, Cmd.none )

        ( GameInit gameInitInfo, EntryPlayer playerID ) ->
            ( gameInitInfo |> entryPlayer playerID |> GameInit, Cmd.none )

        ( GameInit gameInitInfo, ExitPlayer playerID ) ->
            ( gameInitInfo |> exitPlayer playerID |> GameInit, Cmd.none )

        ( GameInit gameInitInfo, StartGame ) ->
            ( initGame gameInitInfo |> GamePlay
            , Cmd.batch
                [ Task.perform GetTurnStartTime Time.now
                , Random.generate InitOrderIDs (gameInitInfo.playerIDs |> Random.List.shuffle)
                ]
            )

        ( GamePlay gamePlayInfo, Tick posix ) ->
            ( { gamePlayInfo | nowTime = posix } |> doTimeRelatedEvents |> GamePlay, Cmd.none )

        ( GamePlay gamePlayInfo, GetTurnStartTime posix ) ->
            ( { gamePlayInfo | turnStartTime = posix, nowTime = posix } |> GamePlay, Cmd.none )

        ( GamePlay gamePlayInfo, InitOrderIDs orderIDs ) ->
            ( initOrderIDs orderIDs gamePlayInfo |> GamePlay, Cmd.none )

        ( GamePlay gamePlayInfo, SelectHoldPit pitNumber ) ->
            ( selectHoldPit pitNumber gamePlayInfo |> GamePlay, Cmd.none )

        ( GamePlay gamePlayInfo, Sowing ) ->
            ( gamePlayInfo
                |> sowingAtOnce
                |> (\gameSowedInfo ->
                        let
                            turnPlayerInfo =
                                getPlayerInfo gameSowedInfo.turnPlayerID gameSowedInfo.playerInfoTable
                        in
                        if getPlayerIsWin turnPlayerInfo then
                            GameEnd (toGameEnd gameSowedInfo)

                        else
                            GamePlay (moveTurn gameSowedInfo)
                   )
            , Cmd.none
            )

        ( GameEnd gameEndInfo, RestartGame ) ->
            ( gameEndInfo |> restartGame |> initGame |> GamePlay
            , Cmd.batch
                [ Task.perform GetTurnStartTime Time.now
                , Random.generate InitOrderIDs (gameEndInfo.playerInfoTable |> Dict.keys |> Random.List.shuffle)
                ]
            )

        ( GameEnd _, InitGame ) ->
            ( dammyGameInitInfo |> GameInit, Cmd.none )

        _ ->
            ( model, Cmd.none )


entryPlayer : PlayerID -> GameInitInfo -> GameInitInfo
entryPlayer playerID gameInitInfo =
    { gameInitInfo | playerIDs = List.append gameInitInfo.playerIDs [ playerID ], entryPlayerName = "" }


exitPlayer : PlayerID -> GameInitInfo -> GameInitInfo
exitPlayer exitPlayerID gameInitInfo =
    { gameInitInfo | playerIDs = List.Extra.remove exitPlayerID gameInitInfo.playerIDs }


initOrderIDs : List PlayerID -> GamePlayInfo -> GamePlayInfo
initOrderIDs orderIDs gamePlayInfo =
    let
        -- playerIDsを先頭を後ろにやって、1つずつずらしたもの
        nextOrderIDs =
            let
                head =
                    orderIDs |> List.head |> Maybe.map List.singleton |> Maybe.withDefault []

                tail =
                    orderIDs |> List.tail |> Maybe.withDefault []
            in
            tail ++ head

        orderIDTable =
            List.map2 (\playerID nextOrderID -> ( playerID, nextOrderID )) orderIDs nextOrderIDs
                |> Dict.fromList
    in
    { gamePlayInfo
        | nextOrderIDTable = orderIDTable
        , cliantID =
            orderIDs
                |> List.head
                |> Maybe.withDefault defaultPlayerID
        , turnPlayerID =
            orderIDs
                |> List.head
                |> Maybe.withDefault defaultPlayerID
    }


initGame : GameInitInfo -> GamePlayInfo
initGame gameInitInfo =
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
            gameInitInfo.playerIDs
    in
    { playerInfoTable =
        playerIDs
            |> List.map (\playerID -> ( playerID, initPlayerInfo playerID ))
            |> Dict.fromList
    , pitCount = gameInitInfo.pitCount
    , initSeedCount = gameInitInfo.initSeedCount
    , turnCount = 1
    , timeLimitForSecond = gameInitInfo.timeLimitForSecond
    , nextOrderIDTable = Dict.empty
    , cliantID = defaultPlayerID
    , turnPlayerID = defaultPlayerID
    , nowTime = Time.millisToPosix 0
    , turnStartTime = Time.millisToPosix 0
    , holdPit = Nothing
    , sowedHole = Nothing
    }


selectHoldPit : PitNumber -> GamePlayInfo -> GamePlayInfo
selectHoldPit newSelectedPitNumber gamePlayInfo =
    let
        newHoldPit =
            Just
                { pitNumber = newSelectedPitNumber
                , seedCount =
                    getPlayerInfo gamePlayInfo.turnPlayerID gamePlayInfo.playerInfoTable
                        |> .pitSeedCounts
                        |> Array.get newSelectedPitNumber
                        |> Maybe.withDefault -1
                }

        turnPlayerInfo =
            getPlayerInfo gamePlayInfo.turnPlayerID gamePlayInfo.playerInfoTable

        selectedPitSeedCount =
            turnPlayerInfo.pitSeedCounts
                |> Array.get newSelectedPitNumber
                |> Maybe.withDefault -1
    in
    if selectedPitSeedCount == 0 then
        gamePlayInfo

    else
        case gamePlayInfo.holdPit of
            Nothing ->
                let
                    -- pitからseedをholdPitへ
                    updatePit : PlayerInfo -> PlayerInfo
                    updatePit playerInfo =
                        { playerInfo
                            | pitSeedCounts =
                                playerInfo.pitSeedCounts
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
                if newSelectedPitNumber == holdPit.pitNumber then
                    gamePlayInfo

                else
                    -- 現在選んでいるpitへseedを戻し、別のpitを選んでseedをholdPitへ
                    let
                        updatePit : PlayerInfo -> PlayerInfo
                        updatePit playerInfo =
                            { playerInfo
                                | pitSeedCounts =
                                    playerInfo.pitSeedCounts
                                        --元のpitへseedを戻す処理
                                        |> Array.set holdPit.pitNumber holdPit.seedCount
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


sowingAtOnce : GamePlayInfo -> GamePlayInfo
sowingAtOnce gamePlayInfo =
    --アニメーションなしに一度にsowing
    case ( gamePlayInfo.holdPit, gamePlayInfo.sowedHole ) of
        ( Just holdPit, Nothing ) ->
            --sowing用の初期化処理
            { gamePlayInfo
                | sowedHole = Just { playerID = gamePlayInfo.turnPlayerID, holeNumber = Pit holdPit.pitNumber }
            }
                |> sowingAtOnce

        ( Just holdPit, Just _ ) ->
            if holdPit.seedCount == 0 then
                -- sowing終了
                gamePlayInfo

            else
                -- sowingつづける
                sowingOneStep gamePlayInfo |> sowingAtOnce

        _ ->
            gamePlayInfo


sowingOneStep : GamePlayInfo -> GamePlayInfo
sowingOneStep gamePlayInfo =
    --holdPitのseed一つだけsowing
    case ( gamePlayInfo.holdPit, gamePlayInfo.sowedHole ) of
        ( Just holdPit, Just prevSowedHole ) ->
            let
                sowingHoleNumber =
                    case prevSowedHole.holeNumber of
                        Store ->
                            Pit 0

                        Pit pitNumber ->
                            if pitNumber + 1 == gamePlayInfo.pitCount then
                                Store

                            else
                                Pit (pitNumber + 1)
            in
            case sowingHoleNumber of
                Store ->
                    let
                        addStoreSeed : PlayerInfo -> PlayerInfo
                        addStoreSeed playerInfo =
                            { playerInfo | storeSeedCount = playerInfo.storeSeedCount + 1 }
                    in
                    { gamePlayInfo
                        | playerInfoTable =
                            gamePlayInfo.playerInfoTable
                                |> Dict.update prevSowedHole.playerID (Maybe.map addStoreSeed)
                        , holdPit = Just { holdPit | seedCount = holdPit.seedCount - 1 }

                        -- 次がstoreなので、必ず自分にsowingする
                        , sowedHole = Just { playerID = prevSowedHole.playerID, holeNumber = Store }
                    }

                Pit sowingPitNumber ->
                    let
                        --前のHoleがStoreなら必ず次のプレイヤーに配る
                        sowingPlayerID =
                            case prevSowedHole.holeNumber of
                                Store ->
                                    gamePlayInfo.nextOrderIDTable
                                        |> Dict.get prevSowedHole.playerID
                                        |> Maybe.withDefault defaultPlayerID

                                Pit _ ->
                                    prevSowedHole.playerID

                        isSowedToTurnPlayer =
                            sowingPlayerID == gamePlayInfo.turnPlayerID

                        isSowedToTurnPlayerPit =
                            sowingPitNumber == holdPit.pitNumber
                    in
                    if isSowedToTurnPlayer && isSowedToTurnPlayerPit then
                        -- 配る先が自分の取ったpitなら飛ばす
                        { gamePlayInfo
                            | sowedHole = Just { playerID = sowingPlayerID, holeNumber = sowingHoleNumber }
                        }

                    else
                        -- 配る先がpitなら普通に配る
                        let
                            addPitSeed : PlayerInfo -> PlayerInfo
                            addPitSeed playerInfo =
                                { playerInfo | pitSeedCounts = playerInfo.pitSeedCounts |> Array.Extra.update sowingPitNumber (\pitCount -> pitCount + 1) }
                        in
                        { gamePlayInfo
                            | playerInfoTable =
                                gamePlayInfo.playerInfoTable
                                    |> Dict.update sowingPlayerID (Maybe.map addPitSeed)
                            , holdPit = Just { holdPit | seedCount = holdPit.seedCount - 1 }
                            , sowedHole = Just { playerID = sowingPlayerID, holeNumber = sowingHoleNumber }
                        }

        _ ->
            gamePlayInfo


toGameEnd : GamePlayInfo -> GameEndInfo
toGameEnd gamePlayInfo =
    { playerInfoTable = gamePlayInfo.playerInfoTable
    , nextOrderIDTable = gamePlayInfo.nextOrderIDTable
    , pitCount = gamePlayInfo.pitCount
    , initSeedCount = gamePlayInfo.initSeedCount
    , cliantID = gamePlayInfo.cliantID
    , turnCount = gamePlayInfo.turnCount
    , reminingTime = getReminingTime gamePlayInfo.turnStartTime gamePlayInfo.nowTime gamePlayInfo.timeLimitForSecond
    , timeLimitForSecond = gamePlayInfo.timeLimitForSecond
    , winnerID = gamePlayInfo.turnPlayerID
    }


moveTurn : GamePlayInfo -> GamePlayInfo
moveTurn gamePlayInfo =
    let
        sowedHoleNumber =
            gamePlayInfo.sowedHole
                |> Maybe.map .holeNumber
                |> Maybe.withDefault (Pit -1)
    in
    case sowedHoleNumber of
        Store ->
            -- 最後に配ったのがstoreならmulti lap
            { gamePlayInfo
                | holdPit = Nothing
                , sowedHole = Nothing
                , turnStartTime = gamePlayInfo.nowTime
            }

        Pit _ ->
            -- 最後に配ったのがpitならターン交代
            { gamePlayInfo
                | holdPit = Nothing
                , sowedHole = Nothing
                , turnPlayerID = getNextOrderID gamePlayInfo.turnPlayerID gamePlayInfo.nextOrderIDTable
                , turnCount = gamePlayInfo.turnCount + 1
                , turnStartTime = gamePlayInfo.nowTime
            }


doTimeRelatedEvents : GamePlayInfo -> GamePlayInfo
doTimeRelatedEvents gamePlayInfo =
    if getReminingTime gamePlayInfo.turnStartTime gamePlayInfo.nowTime gamePlayInfo.timeLimitForSecond == 0 then
        -- 制限時間が過ぎていればターン交代
        { gamePlayInfo
            | holdPit = Nothing
            , sowedHole = Nothing
            , turnPlayerID = getNextOrderID gamePlayInfo.turnPlayerID gamePlayInfo.nextOrderIDTable
            , turnCount = gamePlayInfo.turnCount + 1
            , turnStartTime = gamePlayInfo.nowTime
        }

    else
        gamePlayInfo


restartGame : GameEndInfo -> GameInitInfo
restartGame gameEndInfo =
    { playerIDs = gameEndInfo.playerInfoTable |> Dict.keys
    , entryPlayerName = ""
    , pitCount = gameEndInfo.pitCount
    , initSeedCount = gameEndInfo.initSeedCount
    , timeLimitForSecond = gameEndInfo.timeLimitForSecond
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 Tick



-- VIEW


validateNumberString : String -> Int
validateNumberString nStr =
    nStr |> String.toInt |> Maybe.withDefault 0


viewNumberOption : Int -> Int -> Html Msg
viewNumberOption selectedN n =
    option [ value (String.fromInt n), selected (n == selectedN) ] [ text (String.fromInt n) ]


viewPitCount : GameInitInfo -> Html Msg
viewPitCount gameInitInfo =
    let
        options =
            List.range 3 12
                |> List.map (viewNumberOption gameInitInfo.pitCount)
    in
    div []
        [ text "pit count "
        , select [ onInput (validateNumberString >> SelectPitCount) ] options
        ]


viewInitSeedCount : GameInitInfo -> Html Msg
viewInitSeedCount gameInitInfo =
    let
        options =
            List.range 3 6
                |> List.map (viewNumberOption gameInitInfo.initSeedCount)
    in
    div []
        [ text "seed count "
        , select [ onInput (validateNumberString >> SelectSeedCount) ] options
        ]


viewTimeLimit : GameInitInfo -> Html Msg
viewTimeLimit gameInitInfo =
    let
        options =
            List.range 1 12
                |> List.map (\n -> n * 10)
                |> List.map (viewNumberOption gameInitInfo.timeLimitForSecond)
    in
    div []
        [ text "limit time "
        , select [ onInput (validateNumberString >> SelectTimeLimit) ] options
        ]


viewPlayerIDs : GameInitInfo -> Html Msg
viewPlayerIDs gameInitInfo =
    let
        viewPlayerID : PlayerID -> Html Msg
        viewPlayerID playerID =
            div [] [ text playerID, button [ onClick (ExitPlayer playerID) ] [ text "-" ] ]
    in
    div [] (gameInitInfo.playerIDs |> List.map viewPlayerID)


viewEntryPlayer : GameInitInfo -> Html Msg
viewEntryPlayer gameInitInfo =
    let
        playerNumber =
            gameInitInfo.playerIDs
                |> List.length
                |> String.fromInt

        entryPlayerID =
            gameInitInfo.entryPlayerName ++ "@" ++ playerNumber
    in
    div []
        [ input [ placeholder "put entry player name", value gameInitInfo.entryPlayerName, onInput InputPlayerName ] []
        , button [ onClick (EntryPlayer entryPlayerID) ] [ text "+" ]
        ]


viewStartButton : GameInitInfo -> Html Msg
viewStartButton gameInitInfo =
    if List.length gameInitInfo.playerIDs < 2 then
        div [] []

    else
        button [ onClick StartGame ] [ text "game start" ]


viewBoard : GamePlayInfo -> Html Msg
viewBoard gamePlayInfo =
    let
        viewPlayer : PlayerInfo -> Html Msg
        viewPlayer playerInfo =
            let
                playerIDText =
                    text ("playerID " ++ playerInfo.playerID |> String.padRight 20 '.')

                storeText =
                    text ("{" ++ String.fromInt playerInfo.storeSeedCount ++ "}")
            in
            if playerInfo.playerID == gamePlayInfo.turnPlayerID then
                case gamePlayInfo.holdPit of
                    Just holdPit ->
                        let
                            viewPitButtonOrHold : PitNumber -> Int -> Html Msg
                            viewPitButtonOrHold viewPitNumber seedCount =
                                if holdPit.pitNumber == viewPitNumber then
                                    text ("[" ++ String.fromInt holdPit.seedCount ++ "]")

                                else
                                    button [ onClick (SelectHoldPit viewPitNumber) ] [ text (String.fromInt seedCount) ]

                            pitButtonOrHolds =
                                playerInfo.pitSeedCounts
                                    |> Array.indexedMap viewPitButtonOrHold
                                    |> Array.toList

                            holdText =
                                text (" -- hold " ++ String.fromInt holdPit.pitNumber)
                        in
                        div [] (playerIDText :: pitButtonOrHolds ++ [ storeText, holdText ])

                    Nothing ->
                        let
                            viewPitButton : PitNumber -> Int -> Html Msg
                            viewPitButton pitNumber seedCount =
                                button [ onClick (SelectHoldPit pitNumber) ] [ text (String.fromInt seedCount ++ " ") ]

                            pitButtons =
                                playerInfo.pitSeedCounts
                                    |> Array.indexedMap viewPitButton
                                    |> Array.toList
                        in
                        div [] (playerIDText :: pitButtons ++ [ storeText ])

            else
                let
                    pitText =
                        playerInfo.pitSeedCounts
                            |> Array.map String.fromInt
                            |> Array.map (\pit -> "[" ++ pit ++ "]")
                            |> Array.toList
                            |> String.concat
                            |> String.Extra.wrapWith 3 ","
                            |> text
                in
                div [] [ playerIDText, pitText, storeText ]
    in
    getOrderedIDs gamePlayInfo.cliantID gamePlayInfo.nextOrderIDTable
        |> List.map (\orderedID -> getPlayerInfo orderedID gamePlayInfo.playerInfoTable)
        |> List.map viewPlayer
        |> div []


viewTurnPlayer : GamePlayInfo -> Html Msg
viewTurnPlayer gamePlayInfo =
    div [] [ text ("turnPlayer : " ++ gamePlayInfo.turnPlayerID) ]


viewReminingTimer : GamePlayInfo -> Html Msg
viewReminingTimer gamePlayInfo =
    let
        diffTime =
            getReminingTime gamePlayInfo.turnStartTime gamePlayInfo.nowTime gamePlayInfo.timeLimitForSecond

        reminingTime =
            if diffTime < 0 then
                0

            else
                diffTime
    in
    div [] [ text ("reminingTime : " ++ String.fromInt reminingTime) ]


viewSowingButton : GamePlayInfo -> Html Msg
viewSowingButton gamePlayInfo =
    let
        isHidden =
            case gamePlayInfo.holdPit of
                Nothing ->
                    True

                Just _ ->
                    False
    in
    button [ hidden isHidden, onClick Sowing ] [ text "sowing" ]


viewEndBoard : GameEndInfo -> Html Msg
viewEndBoard gameEndInfo =
    let
        viewPlayer : PlayerInfo -> Html Msg
        viewPlayer playerInfo =
            let
                playerIDText =
                    text ("playerID " ++ playerInfo.playerID ++ "   ")

                storeText =
                    text ("[" ++ String.fromInt playerInfo.storeSeedCount ++ "] ")

                pitTexts =
                    playerInfo.pitSeedCounts
                        |> Array.map (\seedCount -> text (String.concat [ "[" ++ String.fromInt seedCount, "] " ]))
                        |> Array.toList
            in
            div [] (playerIDText :: pitTexts ++ [ storeText ])
    in
    getOrderedIDs gameEndInfo.cliantID gameEndInfo.nextOrderIDTable
        |> List.map (\orderedID -> getPlayerInfo orderedID gameEndInfo.playerInfoTable)
        |> List.map viewPlayer
        |> div []


view : Model -> Html Msg
view model =
    case model of
        GameInit gameInitInfo ->
            div []
                -- TODO ゲームの初期設定用ボタンの追加
                [ text "Game Setting"
                , viewPitCount gameInitInfo
                , viewInitSeedCount gameInitInfo
                , viewTimeLimit gameInitInfo
                , viewPlayerIDs gameInitInfo
                , viewEntryPlayer gameInitInfo
                , viewStartButton gameInitInfo
                ]

        GameEnd gameEndInfo ->
            div []
                [ text "Game End"
                , text ("Game! Winner : " ++ gameEndInfo.winnerID)
                , viewEndBoard gameEndInfo
                , button [ onClick RestartGame ] [ text "restart game!" ]
                , button [ onClick InitGame ] [ text "init game!" ]
                ]

        GamePlay gamePlayInfo ->
            div []
                [ text "Game Playing"
                , viewTurnPlayer gamePlayInfo
                , viewReminingTimer gamePlayInfo
                , viewBoard gamePlayInfo
                , viewSowingButton gamePlayInfo
                ]
