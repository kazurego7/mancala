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


type alias HoldPitInfo =
    { pitNumber : PitNumber
    , seedCount : Int
    }


type alias SowedInfo =
    { playerID : PlayerID
    , holeNumber : HoleNumber
    }


type alias GamePlayInfo =
    { playerInfoTable : Dict PlayerID PlayerInfo
    , nextOrderIDTable : Dict PlayerID PlayerID
    , pitCount : Int
    , holdPit : Maybe HoldPitInfo
    , sowedHole : Maybe SowedInfo
    , cliantID : PlayerID
    , turnPlayerID : PlayerID
    , turnCount : Int
    , nowTime : Time.Posix
    , turnStartTime : Time.Posix
    , timeLimitForSecond : Int
    , winnerID : Maybe PlayerID
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
        |> List.Extra.scanl (\_ prevPlayerID -> getNextOrderID prevPlayerID gamePlayInfo) gamePlayInfo.cliantID


getReminingTime : GamePlayInfo -> Int
getReminingTime gamePlayInfo =
    let
        elapsedSecond =
            Time.Extra.diff Time.Extra.Second Time.utc gamePlayInfo.turnStartTime gamePlayInfo.nowTime
    in
    gamePlayInfo.timeLimitForSecond - elapsedSecond


type alias Model =
    GamePlayInfo


dammyGameInitInfo : GameInitInfo
dammyGameInitInfo =
    { playerIDs = Set.empty |> Set.insert "foo" |> Set.insert "bar"
    , pitCount = 3
    , initSeedCount = 4
    , timeLimitForSecond = 40
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( initGamePlayInfo dammyGameInitInfo
    , Cmd.batch
        [ Task.perform GetGameStartTime Time.now
        , Random.generate InitOrderIDs (dammyGameInitInfo.playerIDs |> Set.toList |> Random.List.shuffle)
        ]
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | GetGameStartTime Time.Posix
    | InitOrderIDs (List PlayerID)
    | SelectHoldPit PitNumber
    | Sowing
    | NextGame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick posix ->
            ( { model | nowTime = posix } |> doTimeRelatedEvents, Cmd.none )

        GetGameStartTime posix ->
            ( { model | turnStartTime = posix, nowTime = posix }, Cmd.none )

        InitOrderIDs orderIDs ->
            ( initOrderIDs orderIDs model, Cmd.none )

        SelectHoldPit pitNumber ->
            ( selectHoldPit pitNumber model, Cmd.none )

        Sowing ->
            ( model |> sowingAtOnce |> moveTurn, Cmd.none )

        NextGame ->
            ( initGamePlayInfo dammyGameInitInfo
            , Cmd.batch
                [ Task.perform GetGameStartTime Time.now
                , Random.generate InitOrderIDs (dammyGameInitInfo.playerIDs |> Set.toList |> Random.List.shuffle)
                ]
            )


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
    in
    { playerInfoTable =
        playerIDs
            |> List.map (\playerID -> ( playerID, initPlayerInfo playerID ))
            |> Dict.fromList
    , pitCount = gameInitInfo.pitCount
    , turnCount = 1
    , timeLimitForSecond = gameInitInfo.timeLimitForSecond
    , nextOrderIDTable = Dict.empty
    , cliantID = defaultPlayerID
    , turnPlayerID = defaultPlayerID
    , nowTime = Time.millisToPosix 0
    , turnStartTime = Time.millisToPosix 0
    , holdPit = Nothing
    , sowedHole = Nothing
    , winnerID = Nothing
    }


selectHoldPit : PitNumber -> GamePlayInfo -> GamePlayInfo
selectHoldPit newSelectedPitNumber gamePlayInfo =
    let
        newHoldPit =
            Just
                { pitNumber = newSelectedPitNumber
                , seedCount =
                    getPlayerInfo gamePlayInfo.turnPlayerID gamePlayInfo
                        |> .pitSeedCounts
                        |> Array.get newSelectedPitNumber
                        |> Maybe.withDefault -1
                }

        turnPlayerInfo =
            getPlayerInfo gamePlayInfo.turnPlayerID gamePlayInfo

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


moveTurn : GamePlayInfo -> GamePlayInfo
moveTurn gamePlayInfo =
    let
        sowedHoleNumber =
            gamePlayInfo.sowedHole
                |> Maybe.map .holeNumber
                |> Maybe.withDefault (Pit -1)

        turnPlayerIsWin =
            gamePlayInfo
                |> getPlayerInfo gamePlayInfo.turnPlayerID
                |> .pitSeedCounts
                |> Array.toList
                |> List.all (\seedCount -> seedCount == 0)
    in
    if turnPlayerIsWin then
        { gamePlayInfo | holdPit = Nothing, sowedHole = Nothing, winnerID = Just gamePlayInfo.turnPlayerID }

    else
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
                    , turnPlayerID = getNextOrderID gamePlayInfo.turnPlayerID gamePlayInfo
                    , turnCount = gamePlayInfo.turnCount + 1
                    , turnStartTime = gamePlayInfo.nowTime
                }


doTimeRelatedEvents : GamePlayInfo -> GamePlayInfo
doTimeRelatedEvents gamePlayInfo =
    if getReminingTime gamePlayInfo == 0 then
        -- 制限時間が過ぎていればターン交代
        { gamePlayInfo
            | holdPit = Nothing
            , sowedHole = Nothing
            , turnPlayerID = getNextOrderID gamePlayInfo.turnPlayerID gamePlayInfo
            , turnCount = gamePlayInfo.turnCount + 1
            , turnStartTime = gamePlayInfo.nowTime
        }

    else
        gamePlayInfo



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 Tick



-- VIEW


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
    getOrderedIDs gamePlayInfo
        |> List.map (\orderedID -> getPlayerInfo orderedID gamePlayInfo)
        |> List.map viewPlayer
        |> div []


viewTurnPlayer : GamePlayInfo -> Html Msg
viewTurnPlayer gamePlayInfo =
    div [] [ text ("turnPlayer : " ++ gamePlayInfo.turnPlayerID) ]


viewReminingTimer : GamePlayInfo -> Html Msg
viewReminingTimer gamePlayInfo =
    let
        diffTime =
            getReminingTime gamePlayInfo

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


viewEndBoard : GamePlayInfo -> Html Msg
viewEndBoard gamePlayInfo =
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
    getOrderedIDs gamePlayInfo
        |> List.map (\orderedID -> getPlayerInfo orderedID gamePlayInfo)
        |> List.map viewPlayer
        |> div []


view : Model -> Html Msg
view model =
    case model.winnerID of
        Just winnerID ->
            div [] [ text ("Game! Winner : " ++ winnerID), viewEndBoard model, button [ onClick NextGame ] [ text "next game" ] ]

        Nothing ->
            div [] [ viewTurnPlayer model, viewReminingTimer model, viewBoard model, viewSowingButton model ]
