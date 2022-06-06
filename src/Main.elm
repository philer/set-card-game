port module Main exposing (main)

import Array exposing (Array)
import Array.Extra as Array
import Browser
import Browser.Dom as Dom
import Browser.Events as Events
import FontAwesome.Icon as FA
import FontAwesome.Regular as FA
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Html.Lazy exposing (lazy3, lazy5)
import Json.Decode as JD
import Json.Encode as JE
import List.Extra as List
import Random
import Random.List exposing (choose, shuffle)
import Set exposing (Set)
import Svg
import Svg.Attributes as SvgA
import Task


banner : String
banner =
    """             Welcome to
           ┌───┬───┬─────┬┐
           │ ──┤ ─┬┴─┐ ┌┬┘│
          ┌┴── │ ─┴─┐│ │├─┤
          └────┴────┘└─┘└─┘
       Created with ❤ and Elm.
https://github.com/philer/set-card-game"""



-- MODEL


type alias Model =
    { gameStatus : GameStatus
    , cardSize : Float
    , players : Array Player
    , colors : List Color
    , showColorSelection : Bool
    , showRules : Bool
    }


type alias GameState =
    { deck : List Card
    , cards : List Card
    , validTriple : List Card
    , selectedCards : List Card
    , hintCards : List Card
    }


type GameStatus
    = Preparation
    | Started GameState
    | Over (List Card)


type alias Player =
    { name : String
    , score : Int
    , blocked : Bool
    }


newPlayer : String -> Player
newPlayer name =
    Player name 0 False


type alias Card =
    Int


type alias Shape =
    String


shapes : Array Shape
shapes =
    Array.fromList [ "rectangle", "tilde", "ellipse" ]


getShape : Card -> Shape
getShape card =
    Array.get (modBy 10 card) shapes
        |> Maybe.withDefault "INVALID_SHAPE"


type alias Pattern =
    String


patterns : Array Pattern
patterns =
    Array.fromList [ "full", "half", "empty" ]


getPattern : Card -> Pattern
getPattern card =
    Array.get (card // 10 |> modBy 10) patterns
        |> Maybe.withDefault "INVALID_PATTERN"


type alias Color =
    String


colorChoices : List Color
colorChoices =
    [ "red"
    , "green"
    , "blue"
    , "purple"
    , "yellow"
    , "cyan"
    , "grey"
    ]


getColor : List Color -> Card -> Color
getColor colors card =
    List.getAt (card // 100 |> modBy 10) colors
        |> Maybe.withDefault "grey"


type alias Count =
    Int


getCount : Card -> Count
getCount card =
    (card // 1000 |> modBy 10) + 1


generateDeck : List Card
generateDeck =
    let
        toBase : Int -> Int -> List Int
        toBase base number =
            case number // base of
                0 ->
                    [ number ]

                _ ->
                    modBy base number :: toBase base (number // base)

        fromBase : Int -> List Int -> Int
        fromBase base =
            List.indexedFoldl (\place digit n -> n + digit * base ^ place) 0
    in
    List.range 0 80
        |> List.map (toBase 3 >> fromBase 10)


{-| 0000, 0003, 0006, 0030, ..., 6666
-}
generateValidTripleSums : Set Card
generateValidTripleSums =
    Set.fromList (List.map (\card -> card * 3) generateDeck)


checkTriple : List Card -> Bool
checkTriple cards =
    List.length cards == 3 && Set.member (List.sum cards) generateValidTripleSums



-- PORTS


port consoleLogPort : JE.Value -> Cmd msg


port consoleErrPort : JE.Value -> Cmd msg


consoleLog : String -> Cmd msg
consoleLog =
    JE.string >> consoleLogPort


consoleErr : String -> Cmd msg
consoleErr =
    JE.string >> consoleErrPort


port playerNamesPort : JE.Value -> Cmd msg


storePlayerNames : Array Player -> Cmd msg
storePlayerNames =
    JE.array (.name >> JE.string) >> playerNamesPort


port colorsPort : JE.Value -> Cmd msg


storeColors : List Color -> Cmd msg
storeColors =
    JE.list JE.string >> colorsPort



-- MAIN


type alias Args =
    { playerNames : Maybe (List String)
    , colors : Maybe (List Color)
    }


main : Program Args Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = \model -> { title = "SET!", body = view model }
        , subscriptions = \_ -> Events.onResize (\_ _ -> WindowResize)
        }


updateCardSize : Cmd Msg
updateCardSize =
    Task.attempt SetCardSize (Dom.getElement "cards")


init : Args -> ( Model, Cmd Msg )
init { playerNames, colors } =
    ( { gameStatus = Preparation
      , cardSize = 200 -- arbitrary value
      , players =
            playerNames
                |> Maybe.withDefault [ "Player 1", "Player 2" ]
                |> List.map newPlayer
                |> Array.fromList
      , colors = colors |> Maybe.withDefault [ "red", "green", "blue" ]
      , showColorSelection = False
      , showRules = False
      }
    , Cmd.batch
        [ consoleLog banner
        , updateCardSize
        ]
    )


type Msg
    = NoOp
    | WindowResize
    | SetCardSize (Result Dom.Error Dom.Element)
    | ToggleRules
    | ToggleColorSelection
    | SelectColor Color
      --| SetColors
      -- player management
    | AddPlayer
    | RemovePlayer Int
    | SetPlayername Int String
      -- game state
    | NewGame
    | RequestStartGame
      -- game progress
    | StartGame (List Card)
    | RequestHint
    | AddHint Card
    | SelectCard Card
    | MakeGuess Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        WindowResize ->
            ( model, updateCardSize )

        SetCardSize (Err (Dom.NotFound error)) ->
            ( model, consoleErr <| "Error while retrieving #cards element: " ++ error )

        SetCardSize (Ok { element }) ->
            let
                ratio =
                    1.1

                ( w, h ) =
                    if element.width / element.height < ratio then
                        ( element.width, element.width / ratio )

                    else
                        ( element.height * ratio, element.height )

                gap =
                    w * 0.01

                height =
                    (h - gap * 4) / 3
            in
            ( { model | cardSize = height }
            , Cmd.none
            )

        ToggleRules ->
            ( { model | showRules = not model.showRules }, Cmd.none )

        ToggleColorSelection ->
            ( { model | showColorSelection = not model.showColorSelection }, Cmd.none )

        SelectColor color ->
            let
                colors =
                    List.drop 1 model.colors ++ [ color ]
            in
            ( { model | colors = colors }
            , storeColors colors
            )

        -- TODO
        AddPlayer ->
            let
                name =
                    "Player " ++ (String.fromInt <| Array.length model.players + 1)

                players =
                    Array.push (newPlayer name) model.players
            in
            ( { model | players = players }
            , updateCardSize
            )

        RemovePlayer index ->
            let
                players =
                    Array.removeAt index model.players
            in
            ( { model | players = players }
            , updateCardSize
            )

        SetPlayername index name ->
            updatePlayerAt (\p -> { p | name = name }) index model

        NewGame ->
            ( { model
                | gameStatus = Preparation
                , players = Array.map (.name >> newPlayer) model.players
              }
            , Cmd.none
            )

        RequestStartGame ->
            ( model
            , Cmd.batch
                [ storePlayerNames model.players
                , Random.generate StartGame (shuffle generateDeck)
                , updateCardSize
                ]
            )

        StartGame deck ->
            ( { model
                | gameStatus =
                    ensureValidTriple <|
                        drawCards 12 <|
                            { deck = deck
                            , cards = []
                            , selectedCards = []
                            , hintCards = []
                            , validTriple = []
                            }
              }
            , Cmd.none
            )

        RequestHint ->
            ( model
            , case model.gameStatus of
                Preparation ->
                    consoleErr "Game has not started yet."

                Over _ ->
                    consoleErr "Game is already over."

                Started gameState ->
                    requestHint gameState
            )

        AddHint card ->
            model
                |> updateStartedGame
                    (\({ selectedCards, hintCards } as gameState) ->
                        { gameState
                            | hintCards = card :: hintCards
                            , selectedCards = card :: hintCards ++ selectedCards |> List.unique |> List.take 3
                        }
                    )

        SelectCard card ->
            model
                |> updateStartedGame
                    (\({ selectedCards } as gameState) ->
                        { gameState
                            | selectedCards =
                                if List.member card selectedCards then
                                    List.remove card selectedCards

                                else
                                    List.take 3 (card :: selectedCards)
                        }
                    )

        MakeGuess playerIndex ->
            makeGuess playerIndex model


updatePlayerAt : (Player -> Player) -> Int -> Model -> ( Model, Cmd Msg )
updatePlayerAt updatePlayer index ({ players } as model) =
    case Array.get index players of
        Nothing ->
            ( model, consoleErr <| "Unknown Player index: " ++ String.fromInt index )

        Just player ->
            ( { model | players = Array.set index (updatePlayer player) players }, Cmd.none )


updateStartedGame : (GameState -> GameState) -> Model -> ( Model, Cmd Msg )
updateStartedGame updateGameState ({ gameStatus } as model) =
    case gameStatus of
        Preparation ->
            ( model, consoleErr "Game has not started yet." )

        Over _ ->
            ( model, consoleErr "Game is already over." )

        Started gameState ->
            ( { model | gameStatus = Started <| updateGameState gameState }, Cmd.none )


makeGuess : Int -> Model -> ( Model, Cmd Msg )
makeGuess playerIndex ({ gameStatus } as model) =
    case gameStatus of
        Preparation ->
            ( model, consoleErr "Game has not started yet." )

        Over _ ->
            ( model, consoleErr "Game is already over." )

        Started ({ cards, selectedCards } as gameState) ->
            if checkTriple selectedCards then
                let
                    newGameState =
                        if List.length cards > 12 then
                            removeSelectedCards gameState

                        else
                            replaceSelectedCards gameState

                    newGameStatus =
                        ensureValidTriple { newGameState | selectedCards = [], hintCards = [] }
                in
                { model | gameStatus = newGameStatus }
                    |> unblockAllPlayers
                    |> updatePlayerAt (\p -> { p | score = p.score + 3 }) playerIndex

            else
                { model | gameStatus = Started { gameState | selectedCards = [] } }
                    |> updatePlayerAt (\p -> { p | blocked = True }) playerIndex
                    |> Tuple.mapFirst checkBlockedPlayers


removeSelectedCards : GameState -> GameState
removeSelectedCards ({ cards, selectedCards } as gameState) =
    { gameState
        | cards = List.filter (\card -> not <| List.member card selectedCards) cards
        , selectedCards = []
        , hintCards = []
    }


replaceSelectedCards : GameState -> GameState
replaceSelectedCards ({ selectedCards } as gameState) =
    let
        replace : Card -> ( List Card, List Card ) -> ( List Card, List Card )
        replace card ( newDeck, newCards ) =
            if List.member card selectedCards then
                case newDeck of
                    newCard :: remainingDeck ->
                        ( remainingDeck, newCard :: newCards )

                    [] ->
                        ( newDeck, newCards )

            else
                ( newDeck, card :: newCards )

        ( deck, cards ) =
            List.foldr replace ( gameState.deck, [] ) gameState.cards
    in
    { gameState | deck = deck, cards = cards }


drawCards : Int -> GameState -> GameState
drawCards count ({ deck, cards } as gameState) =
    { gameState
        | deck = List.drop count deck
        , cards = cards ++ List.take count deck
    }


ensureValidTriple : GameState -> GameStatus
ensureValidTriple ({ cards, deck } as gameState) =
    case findValidTriple cards of
        Just validTriple ->
            Started { gameState | validTriple = validTriple }

        Nothing ->
            if List.length deck == 0 then
                Over cards

            else
                ensureValidTriple <| drawCards 3 gameState


requestHint : GameState -> Cmd Msg
requestHint { validTriple, hintCards } =
    validTriple
        |> List.filter (\card -> not <| List.member card hintCards)
        |> choose
        |> Random.generate
            (\( maybeHintCard, _ ) ->
                case maybeHintCard of
                    Nothing ->
                        NoOp

                    Just hintCard ->
                        AddHint hintCard
            )


unblockAllPlayers : Model -> Model
unblockAllPlayers model =
    { model | players = Array.map (\p -> { p | blocked = False }) model.players }


checkBlockedPlayers : Model -> Model
checkBlockedPlayers ({ players } as model) =
    if Array.foldl (.blocked >> (&&)) True players then
        unblockAllPlayers model

    else
        model


distinctCombinations : Int -> List a -> List (List a)
distinctCombinations len items =
    let
        fatFold : (a -> List a -> b -> b) -> b -> List a -> b
        fatFold fn acc lst =
            case lst of
                [] ->
                    acc

                x :: xs ->
                    fatFold fn (fn x xs acc) xs
    in
    if len <= 0 || items == [] then
        []

    else if len == 1 then
        List.map List.singleton items

    else
        fatFold
            (\x xs acc ->
                List.map ((::) x) (distinctCombinations (len - 1) xs) ++ acc
            )
            []
            items


findValidTriple : List Card -> Maybe (List Card)
findValidTriple =
    -- TODO optimization: lazy
    List.find checkTriple << distinctCombinations 3



-- VIEW


view : Model -> List (Html Msg)
view model =
    [ h1 [] [ text "SET!" ]
    , div [ id "tools" ]
        [ button [ onClick ToggleRules ] [ text "Rules" ]
        , text "·"
        , button [ onClick ToggleColorSelection ] [ text "Colors" ]
        ]
    , viewRules model
    , viewColorSelection model
    , viewPlayers model
    , viewCards model
    , viewInfo model
    , svgDefs
    ]


viewPlayers : Model -> Html Msg
viewPlayers { gameStatus, players } =
    div [ id "players" ] <|
        case gameStatus of
            Preparation ->
                let
                    canRemove =
                        Array.length players > 1
                in
                Array.indexedMapToList (viewPlayerInput canRemove) players
                    ++ [ button
                            [ class "paper add-player-button", onClick AddPlayer ]
                            [ text "+" ]
                       ]

            Started { selectedCards } ->
                Array.indexedMapToList (viewPlayer <| List.length selectedCards == 3) players

            Over _ ->
                Array.indexedMapToList (viewPlayer False) players


viewPlayerInput : Bool -> Int -> Player -> Html Msg
viewPlayerInput canRemove index player =
    let
        inputId =
            "player-name-input-" ++ String.fromInt index
    in
    div
        [ class "paper player player-input" ]
    <|
        input
            [ id inputId
            , class "player-name-input"
            , placeholder "Player Name"
            , value player.name
            , onInput (SetPlayername index)
            , onSubmit RequestStartGame
            ]
            []
            :: (if canRemove then
                    [ button [ class "remove-player-button", onClick (RemovePlayer index) ] [ text "✕" ] ]

                else
                    []
               )


viewPlayer : Bool -> Int -> Player -> Html Msg
viewPlayer canGuess index { name, score, blocked } =
    button
        [ class "paper player"
        , disabled blocked
        , onClick <|
            if not canGuess || blocked then
                NoOp

            else
                MakeGuess index
        ]
        [ span [ class "player-name" ] [ text name ]
        , span [ class "player-score" ] [ text (String.fromInt score) ]
        ]


viewCards : Model -> Html Msg
viewCards { gameStatus, cardSize, colors } =
    div
        [ id "cards"
        , class <|
            case gameStatus of
                Preparation ->
                    "game-preparation"

                Started _ ->
                    "game-started"

                Over _ ->
                    "game-over"
        ]
    <|
        case gameStatus of
            Preparation ->
                [ viewCard cardSize colors 2
                , viewCard cardSize colors 1111
                , viewCard cardSize colors 2220
                , button [ class "paper start-button", onClick RequestStartGame ]
                    [ text "Start" ]
                ]

            Started { cards, selectedCards, hintCards } ->
                List.map
                    (\card ->
                        lazy5
                            viewCardButton
                            cardSize
                            colors
                            (List.member card selectedCards)
                            (List.member card hintCards)
                            card
                    )
                    cards

            Over cards ->
                List.map (viewCard cardSize colors) cards
                    ++ [ div [ class "game-over-screen" ]
                            [ div [ class "game-over-text" ] [ text "Game Over" ]
                            , button [ class "paper new-game-button", onClick NewGame ]
                                [ text "Play Again" ]
                            ]
                       ]


viewCard : Float -> List Color -> Card -> Html Msg
viewCard size colors card =
    let
        color =
            getColor colors card
    in
    div
        [ classList
            [ ( "paper", True )
            , ( "card", True )
            , ( "card-" ++ color, True )
            ]
        , style "font-size" <| String.fromFloat size ++ "px"
        ]
    <|
        (List.repeat (getCount card) <| lazy3 shape2svg (getShape card) (getPattern card) color)


viewCardButton : Float -> List Color -> Bool -> Bool -> Card -> Html Msg
viewCardButton size colors selected highlighted card =
    let
        color =
            getColor colors card
    in
    button
        [ classList
            [ ( "paper", True )
            , ( "card", True )
            , ( "card-" ++ color, True )
            , ( "selected", selected )
            ]
        , style "font-size" <| String.fromFloat size ++ "px"
        , onClick <| SelectCard card
        ]
    <|
        (List.repeat (getCount card) <| lazy3 shape2svg (getShape card) (getPattern card) color)
            ++ (if highlighted then
                    [ FA.viewIcon FA.smileWink ]

                else
                    []
               )


viewInfo : Model -> Html Msg
viewInfo { gameStatus } =
    case gameStatus of
        Started { deck, hintCards } ->
            div [ id "info" ]
                [ div [ class "info-text" ]
                    [ text <| (List.length deck |> String.fromInt) ++ " cards left" ]
                , button
                    [ class "paper"
                    , disabled <| List.length hintCards >= 3
                    , onClick RequestHint
                    ]
                    [ text <|
                        if List.length hintCards == 0 then
                            "Hint"

                        else
                            "Next Hint"
                    ]
                ]

        _ ->
            text ""


onDirectClick : String -> msg -> Attribute msg
onDirectClick id msg =
    Html.Events.on "click"
        (JD.at [ "target", "id" ] JD.string
            |> JD.andThen
                (\targetId ->
                    if targetId == id then
                        JD.succeed msg

                    else
                        JD.fail "not a direct click"
                )
        )


viewModal : String -> Bool -> msg -> String -> List (Html msg) -> Html msg
viewModal id open close title content =
    if not open then
        text ""

    else
        div [ class "modal", Html.Attributes.id id, onDirectClick id close ]
            [ div [ class "paper" ]
                [ header []
                    [ h2 [] [ text title ]
                    , button [ class "close-button", onClick close ] [ text "✕" ]
                    ]
                , div [] content
                ]
            ]


viewRules : Model -> Html Msg
viewRules { showRules, colors } =
    let
        smallCard =
            viewCard 120 colors

        viewList : List (Html msg) -> Html msg
        viewList items =
            ul [] <| List.map (li [] << List.singleton) items

        viewExample : List Card -> List String -> Html Msg
        viewExample cards explanations =
            div [ class "example" ] <|
                List.map smallCard cards
                    ++ [ viewList <| List.map text explanations ]
    in
    viewModal "rules-modal"
        showRules
        ToggleRules
        "How to play"
        [ p [] [ text "Be the first to find a set of three cards that have…" ]
        , viewList
            [ text "all the same color or all different colors and"
            , text "all the same shape or all different shapes and"
            , text "all the same pattern or all different patterns and"
            , text "all the same number of symbols or all different numbers of symbols."
            ]
        , p [] [ text "When you have found a set, say “SET!”, select the cards and add them to your name." ]
        , h3 [] [ text "Examples:" ]
        , h4 [] [ text "✔️ Correct set:" ]
        , viewExample [ 222, 1222, 2222 ]
            [ "✔️ All cards have the same color."
            , "✔️ All cards have the same shape."
            , "✔️ All cards have the same pattern."
            , "✔️ All cards have different numbers of symbols."
            ]
        , h4 [] [ text "✔️ Correct set:" ]
        , viewExample [ 1002, 1111, 1220 ]
            [ "✔️ All cards have different colors."
            , "✔️ All cards have different shapes."
            , "✔️ All cards have different patterns."
            , "✔️ All cards have the same number of symbols."
            ]
        , h4 [] [ text "❌ Wrong set:" ]
        , viewExample [ 2221, 1111, 101 ]
            [ "❌ Not all cards have the same color or different colors."
            , "✔️ All cards have different shapes."
            , "✔️ All cards have different patterns."
            , "✔️ All cards have the same number of symbols."
            ]
        ]


viewColorSelection : Model -> Html Msg
viewColorSelection { showColorSelection, colors } =
    viewModal "color-selection-modal"
        showColorSelection
        ToggleColorSelection
        "Select colors"
        [ div [ id "color-choices" ] (List.map (\choice -> viewColorCard (List.member choice colors) choice) colorChoices) ]


viewColorCard : Bool -> Color -> Html Msg
viewColorCard selected color =
    button
        [ classList
            [ ( "paper", True )
            , ( "card", True )
            , ( "card-" ++ color, True )
            , ( "selected", selected )
            ]
        , style "font-size" "120px"
        , onClick <| SelectColor color
        ]
        [ shape2svg "rectangle" "empty" color
        , shape2svg "ellipse" "half" color
        , shape2svg "tilde" "full" color
        ]



-- SVG VIEW


symbolWidth : number
symbolWidth =
    120


symbolHeight : number
symbolHeight =
    50


symbolPadding : number
symbolPadding =
    10


shape2svg : Shape -> Pattern -> Color -> Svg.Svg Msg
shape2svg shape pattern color =
    let
        width =
            symbolWidth + 2 * symbolPadding

        height =
            symbolHeight + 2 * symbolPadding

        viewBox =
            [ 0, 0, width, height ]
    in
    Svg.svg
        [ SvgA.width <| String.fromInt width
        , SvgA.height <| String.fromInt height
        , SvgA.viewBox <| String.join " " (List.map String.fromFloat viewBox)
        ]
        [ useShape shape pattern color ]


useShape : Shape -> Pattern -> Color -> Svg.Svg Msg
useShape shape pattern color =
    Svg.use
        [ SvgA.xlinkHref <| "#shape-" ++ shape -- Svg.Attributes.href doesn't exist
        , SvgA.class <| "shape-" ++ pattern ++ " shape-" ++ color
        ]
        []


svgDefs : Svg.Svg Msg
svgDefs =
    Svg.svg
        [ SvgA.class "svg-defs" ]
        [ Svg.defs [] <|
            [ rectangle, tilde, ellipse ]
                ++ List.map svgPatternHatch colorChoices
        ]


rectangle : Svg.Svg Msg
rectangle =
    Svg.rect
        [ SvgA.id "shape-rectangle"
        , SvgA.class "shape"
        , SvgA.x <| String.fromFloat symbolPadding
        , SvgA.y <| String.fromFloat symbolPadding
        , SvgA.width <| String.fromFloat symbolWidth
        , SvgA.height <| String.fromFloat symbolHeight
        ]
        []


tilde : Svg.Svg Msg
tilde =
    Svg.path
        [ SvgA.id "shape-tilde"
        , SvgA.class "shape"
        , SvgA.d "m44.2 10c-25.3 0-36.4 23.7-32.4 34.6 3.9 10.7 16.5-6.3 31.1-6.8 16.4-0.6 31.1 20.3 52.9 20.3 25.3 0 36.4-23.7 32.4-34.6-3.9-10.7-16.5 6.3-31.1 6.8-16.4 0.6-31.1-20.3-52.9-20.3z"
        ]
        []


ellipse : Svg.Svg Msg
ellipse =
    Svg.ellipse
        [ SvgA.id "shape-ellipse"
        , SvgA.class "shape"
        , SvgA.cx <| String.fromFloat (symbolPadding + symbolWidth / 2)
        , SvgA.cy <| String.fromFloat (symbolPadding + symbolHeight / 2)
        , SvgA.rx <| String.fromFloat (symbolWidth / 2)
        , SvgA.ry <| String.fromFloat (symbolHeight / 2)
        ]
        []


svgPatternHatch : Color -> Svg.Svg Msg
svgPatternHatch color =
    Svg.pattern
        [ SvgA.id <| "pattern-diagonal-hatch-" ++ color
        , SvgA.width "5"
        , SvgA.height "5"
        , SvgA.patternTransform "rotate(35 0 0)"
        , SvgA.patternUnits "userSpaceOnUse"
        ]
        [ Svg.line
            [ SvgA.x1 "0"
            , SvgA.y1 "0"
            , SvgA.x2 "0"
            , SvgA.y2 "5"
            , SvgA.style <| "stroke-width:2.5"

            --, SvgA.class <| "stroke-" ++ color
            ]
            []
        ]
