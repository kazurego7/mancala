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


type alias HoldPit =
    Maybe
        { pitNumber : PitNumber
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


dammyGameInitInfo : GameInitInfo
dammyGameInitInfo =
    { playerIDs = Set.empty |> Set.insert "hoge" |> Set.insert "fuga" |> Set.insert "alice"
    , pitCount = 6
    , initSeedCount = 4
    , timeLimitForSecond = 30
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( initGamePlayInfo dammyGameInitInfo
    , Cmd.batch
        [ Task.perform GameStartTime Time.now
        , Random.generate InitOrderIDs (dammyGameInitInfo.playerIDs |> Set.toList |> Random.List.shuffle)
        ]
    )



-- UPDATE


type Msg
    = GameStartTime Time.Posix
    | InitOrderIDs (List PlayerID)
    | SelectHoldPit PitNumber
    | Sowing
    | NoMsg


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
        , myID =
            orderIDs
                |> List.head
                |> Maybe.withDefault defaultPlayerID
        , turnPlayerID =
            orderIDs
                |> List.head
                |> Maybe.withDefault defaultPlayerID
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GameStartTime posix ->
            ( { model | turnStartTime = posix }, Cmd.none )

        InitOrderIDs orderIDs ->
            ( initOrderIDs orderIDs model, Cmd.none )

        SelectHoldPit pitNumber ->
            ( selectHoldPit pitNumber model, Cmd.none )

        Sowing ->
            ( sowingAtOnce model, Cmd.none )

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
    in
    { playerInfoTable =
        playerIDs
            |> List.map (\playerID -> ( playerID, initPlayerInfo playerID ))
            |> Dict.fromList
    , pitCount = gameInitInfo.pitCount
    , turnCount = 1
    , timeLimitForSecond = gameInitInfo.timeLimitForSecond
    , nextOrderIDTable = Dict.empty
    , myID = defaultPlayerID
    , turnPlayerID = defaultPlayerID
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
        ( Just prevHoldPit, Nothing ) ->
            let
                -- pitの次は、必ず自分のpitかstore
                nextHoleNumber =
                    if prevHoldPit.pitNumber + 1 == gamePlayInfo.pitCount then
                        Store

                    else
                        Pit (prevHoldPit.pitNumber + 1)
            in
            { gamePlayInfo
                | sowedHole = Just { sowedPlayerID = gamePlayInfo.turnPlayerID, sowedHoleNumber = nextHoleNumber }
            }
                |> sowingAtOnce

        ( Just prevHoldPit, Just _ ) ->
            if prevHoldPit.seedCount == 0 then
                gamePlayInfo

            else
                sowingOneStep gamePlayInfo |> sowingAtOnce

        _ ->
            gamePlayInfo


sowingOneStep : GamePlayInfo -> GamePlayInfo
sowingOneStep gamePlayInfo =
    --holdPitのseed一つだけsowing
    case ( gamePlayInfo.holdPit, gamePlayInfo.sowedHole ) of
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

                Pit sowedPitNumber ->
                    let
                        isSowedToTurnPlayer =
                            prevSowedHole.sowedPlayerID == gamePlayInfo.turnPlayerID

                        isSowedToTurnPlayerPit =
                            sowedPitNumber == prevHoldPit.pitNumber

                        -- pitの次は、必ず自分のpitかstore
                        nextHoleNumber =
                            if sowedPitNumber + 1 == gamePlayInfo.pitCount then
                                Store

                            else
                                Pit (sowedPitNumber + 1)
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
                                { playerInfo | pitSeedCounts = playerInfo.pitSeedCounts |> Array.Extra.update sowedPitNumber (\pitCount -> pitCount + 1) }
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
subscriptions _ =
    Sub.none



-- VIEW


viewBoard : GamePlayInfo -> Html Msg
viewBoard gamePlayInfo =
    let
        viewPlayer : PlayerInfo -> Html Msg
        viewPlayer playerInfo =
            let
                storeText =
                    text ("store " ++ String.fromInt playerInfo.storeSeedCount)
            in
            if playerInfo.playerID == gamePlayInfo.turnPlayerID then
                let
                    viewPitButton : PitNumber -> Int -> Html Msg
                    viewPitButton pitNumber seedCount =
                        button [ onClick (SelectHoldPit pitNumber) ] [ text (String.concat [ "pit", String.fromInt pitNumber, " ", "seed", String.fromInt seedCount, " " ]) ]

                    pitButtons =
                        playerInfo.pitSeedCounts
                            |> Array.indexedMap viewPitButton
                            |> Array.toList
                in
                case gamePlayInfo.holdPit of
                    Just holdPit ->
                        let
                            holdPitText =
                                text ("hold pit" ++ String.fromInt holdPit.pitNumber ++ " " ++ String.fromInt holdPit.seedCount)
                        in
                        div [] (pitButtons ++ [ storeText, text " ", holdPitText ])

                    Nothing ->
                        div [] (pitButtons ++ [ storeText ])

            else
                let
                    pitTexts =
                        playerInfo.pitSeedCounts
                            |> Array.indexedMap (\pitNumber seedCount -> text (String.concat [ "pit", String.fromInt pitNumber, " ", "seed", String.fromInt seedCount ]))
                            |> Array.toList
                in
                div [] (pitTexts ++ [ storeText ])
    in
    getOrderedIDs gamePlayInfo
        |> List.map (\orderedID -> getPlayerInfo orderedID gamePlayInfo)
        |> List.map viewPlayer
        |> div []


viewSowingButton : Model -> Html Msg
viewSowingButton model =
    let
        isHidden =
            case model.holdPit of
                Nothing ->
                    True

                Just _ ->
                    False
    in
    button [ hidden isHidden, onClick Sowing ] [ text "sowing" ]


view : Model -> Html Msg
view model =
    div [] [ viewBoard model, viewSowingButton model ]
