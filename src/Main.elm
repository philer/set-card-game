port module Main exposing (..)

import Array exposing (Array)
import Browser
import Browser.Dom as Dom
import Browser.Events as Events
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Json.Decode as JD exposing (decodeString, array, string)
import Json.Encode as JE exposing (encode, array, string, dict)
import Random
import Random.List exposing (shuffle, choose)
import Set exposing (Set)
import Svg
import Svg.Attributes as SvgA
import Task exposing (Task)
import Time

import Array.Extra as Array
import FontAwesome.Icon as FA exposing (Icon)
import FontAwesome.Regular as FA
import List.Extra as List

banner = """             Welcome to
           ┌───┬───┬─────┬┐
           │ ──┤ ─┬┴─┐ ┌┬┘│
          ┌┴── │ ─┴─┐│ │├─┤
          └────┴────┘└─┘└─┘
       Created with ❤ and Elm.
https://github.com/philer/set-card-game"""

-- MODEL

type alias Model =
  { gameState : GameState
  , players : Array Player
  , deck : List Card
  , cards : List Card
  , selectedCards : List CardId
  , hintCards : List CardId
  , validTriple : Maybe (List Card)
  , cardSize : (Float, Float)
  }

type GameState
  = Preparation
  | Started
  | Over

type alias Player =
  { name : String
  , score : Int
  , blocked : Bool
  }

newPlayer name =
  Player name 0 False

type alias CardId = Int

type alias Shape = String
shapes : Array Shape
shapes = Array.fromList [ "rectangle", "tilde", "ellipse" ]

type alias Pattern = String
patterns : Array Pattern
patterns = Array.fromList [ "full", "half", "empty" ]

type alias Color = String
colors : Array Color
colors = Array.fromList [ "red", "green", "blue" ]

type alias Count = Int
counts : Array Count
counts = Array.fromList [ 1, 2, 3 ]

type alias Card =
  { id : CardId
  , shape : Shape
  , pattern : Pattern
  , color : Color
  , count : Count
  }

generateDeck : List Card
generateDeck =
  let
    toBase : Int -> Int -> List Int
    toBase base number =
      case number // base of
        0 -> [ number ]
        x -> modBy base number :: toBase base (number // base)

    fromBase : Int -> List Int -> Int
    fromBase base =
      List.indexedFoldl (\place digit n -> n + digit * base ^ place) 0

    pad : a -> Int -> List a -> List a
    pad item count xs =
      xs ++ List.repeat (count - List.length xs) item

    buildCard : Int -> Card
    buildCard index =
      let
        idxs = toBase 3 index
      in
      case pad 0 4 idxs of
        shapeIndex::patternIndex::colorIndex::countIndex::[] ->
          Card
            (fromBase 10 idxs)
            (Maybe.withDefault "INVALID" <| Array.get shapeIndex shapes)
            (Maybe.withDefault "INVALID" <| Array.get patternIndex patterns)
            (Maybe.withDefault "INVALID" <| Array.get colorIndex colors)
            (Maybe.withDefault -1 <| Array.get countIndex counts)
        _ ->
          Card -1 "INVALID" "INVALID" "INVALID" -1  -- should be impossible
  in
  List.map buildCard <| List.range 0 80

{-| 0000, 0003, 0006, 0030, ..., 6666 -}
validCardTripleSums : Set CardId
validCardTripleSums =
  Set.fromList (List.map (\card -> card.id * 3) generateDeck)

checkTriple : List CardId -> Bool
checkTriple ids =
  Set.member (List.sum ids) validCardTripleSums


-- PORTS


port consoleLogPort : JE.Value -> Cmd msg
port consoleErrPort : JE.Value -> Cmd msg
consoleLog = JE.string >> consoleLogPort
consoleErr = JE.string >> consoleErrPort

port playerNamesPort : JE.Value -> Cmd msg
storePlayerNames : Model -> Cmd msg
storePlayerNames { players } =
  JE.array (.name >> JE.string) players |> playerNamesPort

port gameResultPort : JE.Value -> Cmd msg
storeGameResult : Cmd Msg
storeGameResult =
  Task.perform StoreGameResult Time.now


-- MAIN

main : Program (Maybe (Array String)) Model Msg
main =
  Browser.document
    { init = init
    , update = update
    , view = \model -> { title = "SET!", body = view model }
    , subscriptions = \_ -> Events.onResize (\_ _ -> WindowResize)
    }

msg2cmd : Msg -> Cmd Msg
msg2cmd msg =
  Task.succeed msg |> Task.perform identity

updateCardSize : Cmd Msg
updateCardSize =
  Task.attempt SetCardSize (Dom.getElement "cards")

init : (Maybe (Array String)) -> ( Model, Cmd Msg )
init flags =
  let
    playerNames =
      Maybe.withDefault (Array.fromList [ "Player 1", "Player 2" ]) flags

  in
  ( { gameState = Preparation
    , players = Array.map newPlayer playerNames
    , deck = []
    , cards = []
    , selectedCards = []
    , hintCards = []
    , validTriple = Nothing
    , cardSize = (200, 300)  -- arbitrary value
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
  | NewGame
  | AddPlayer
  | RemovePlayer Int
  | SetPlayername Int String
  | StartGame
  | SetDeck (List Card)
  | CheckImpossible
  | AddHint CardId
  | SelectCard CardId
  | MakeGuess Int
  | StoreGameResult Time.Posix

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case ( msg, model ) of
    ( NoOp, _ ) ->
      ( model, Cmd.none )
    ( WindowResize, _ ) ->
      ( model, updateCardSize )
    ( SetCardSize (Err (Dom.NotFound error)), _ ) ->
      ( model, consoleErr <| "Error while retrieving #cards element: " ++ error )
    ( SetCardSize (Ok { element }), _ ) ->
      let
        ratio = 1.1
        (columns, rows) = (5, 3)
        (w, h) =
          if element.width / element.height < ratio then
            (element.width, element.width / ratio)
          else
            (element.height * ratio, element.height)
        gap = w * 0.01
        width = (w - gap * (columns + 1)) / columns
        height = (h - gap * (rows + 1)) / rows
      in
      ( { model | cardSize = (width, height) }
      , Cmd.none
      )
    ( NewGame, { players } ) ->
      ( { model
        | gameState = Preparation
        , players = Array.map (.name >> newPlayer) players
        , selectedCards = []
        , hintCards = []
        , validTriple = Nothing
        }
      , Cmd.none
      )
    ( AddPlayer, { players } ) ->
      let
        name = "Player " ++ (String.fromInt <| Array.length players + 1)
      in
      ( { model | players = Array.push (newPlayer name) players }
      , updateCardSize
      )
    ( RemovePlayer index, { players } ) ->
      ( { model | players = Array.removeAt index players }
      , updateCardSize
      )
    ( SetPlayername index name, { players } ) ->
      case Array.get index players of
        Just player ->
          ( { model
            | players = Array.set index { player | name = name } players
            }
          , Cmd.none
          )
        Nothing ->
          ( model, Cmd.none )
    ( StartGame, _ ) ->
      ( { model | gameState = Started }
      , Cmd.batch
          [ storePlayerNames model
          , Random.generate SetDeck (shuffle generateDeck)
          , updateCardSize
          ]
      )
    ( SetDeck newDeck, _ ) ->
      ( addCards 12 { model | deck = newDeck, cards = [] }
      , Cmd.none
      )
    ( CheckImpossible, _ ) ->
      checkImpossible model
    ( AddHint cardId, { hintCards, selectedCards } ) ->
      ( { model
        | hintCards = cardId :: hintCards
        , selectedCards =
            cardId :: hintCards ++ selectedCards |> List.unique |> List.take 3
        }
      , Cmd.none
      )
    ( SelectCard cardId, { selectedCards } ) ->
      ( { model
        | selectedCards =
            if List.member cardId selectedCards then
                List.remove cardId selectedCards
            else
              List.take 3 (cardId :: selectedCards)
        }
      , Cmd.none
      )
    ( MakeGuess playerIndex, _ ) ->
      makeGuess playerIndex model
    ( StoreGameResult timestamp, { players } ) ->
      ( model
      , gameResultPort <| JE.object
        [ ( "timestamp", timestamp |> Time.posixToMillis |> JE.int )
        , ( "players"
          , players
              |> Array.mapToList (\{ name, score } -> ( name, JE.int score ))
              |> JE.object
          )
        ]
      )

makeGuess : Int -> Model -> (Model, Cmd Msg)
makeGuess playerIndex ({players, cards, deck, selectedCards} as model) =
  if List.length selectedCards < 3 then
    ( model, Cmd.none )
  else
    case Array.get playerIndex players of
      Nothing ->  -- error, should not be possible
        ( model, consoleErr <| "Unknown Player index: " ++ String.fromInt playerIndex)
      Just player ->
        ( if checkTriple selectedCards then
            let
              (newDeck, newCards) =
                if List.length cards <= 12 then
                  replaceSelectedCards deck selectedCards cards
                else
                  (deck, removeSelectedCards selectedCards cards)
            in
            unblockAllPlayers
              { model
              | players =
                  Array.set
                    playerIndex
                    { player | score = player.score + 3 }
                    players
              , deck = newDeck
              , cards = newCards
              , selectedCards = []
              , hintCards = []
              , validTriple = findValidTriple newCards
              }
          else
            checkBlockedPlayers
              { model
              | players =
                  Array.set
                    playerIndex
                    { player | blocked = True }
                    players
              , selectedCards = []
              }
        , Cmd.none
        )

checkImpossible : Model -> (Model, Cmd Msg)
checkImpossible ({ deck, cards, validTriple, hintCards } as model) =
  Tuple.mapFirst unblockAllPlayers
    <| case validTriple of
      Nothing ->
        if List.length deck == 0 then
          ( { model | gameState = Over }, storeGameResult )
        else
          ( addCards 3 model, Cmd.none )
      Just triple ->
        ( model
        , List.map .id triple
            |> List.filter (\cardId -> not <| List.member cardId hintCards)
            |> choose
            |> Random.generate (\(maybeHintCardId, _) ->
                case maybeHintCardId of
                  Nothing -> NoOp
                  Just hintCardId -> AddHint hintCardId
              )
        )

addCards : Int -> Model -> Model
addCards count ({ deck, cards } as model) =
  let
    newCards = cards ++ List.take count deck
  in
  { model
  | deck = List.drop count deck
  , cards = newCards
  , validTriple = findValidTriple newCards
  }

removeSelectedCards : List CardId -> List Card -> List Card
removeSelectedCards selectedCards =
  List.filter (\card -> not <| List.member card.id selectedCards)

replaceSelectedCards : List Card -> List CardId -> List Card -> (List Card, List Card)
replaceSelectedCards deck selectedCards =
  let
    replace : Card -> (List Card, List Card) -> (List Card, List Card)
    replace card ( newDeck, newCards ) =
      if List.member card.id selectedCards then
        case newDeck of
          newCard :: remainingDeck ->
            ( remainingDeck, newCard :: newCards )
          [] ->
            ( newDeck, newCards )
      else
        ( newDeck, card :: newCards )
  in
  List.foldr replace ( deck, [] )


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
        [] -> acc
        x::xs -> fatFold fn (fn x xs acc) xs
  in
  if len <= 0 || items == [] then
    []
  else if len == 1 then
    List.map List.singleton items
  else
    fatFold
      (\x xs acc ->
        (List.map ((::) x) (distinctCombinations (len - 1) xs)) ++ acc
      )
      []
      items

findValidTriple : List Card -> Maybe (List Card)
findValidTriple cards =
  List.find
    (List.map .id >> checkTriple)
    -- TODO optimization: lazy
    (distinctCombinations 3 cards)


-- VIEW

view : Model -> List (Html Msg)
view model =
  [ h1 [] [ text "SET!" ]
  , viewPlayers model
  , viewCards model
  , viewInfo model
  , svgDefs
  ]


viewPlayers : Model -> Html Msg
viewPlayers { gameState, players } =
  div [ id "players" ]
    <| if gameState == Preparation then
      let
        canRemove = Array.length players > 1
      in
      (Array.indexedMapToList (\i p -> viewPlayerInput i p canRemove) players)
      ++ [ button
            [ class "material add-player-button" , onClick AddPlayer ]
            [ text "+" ]
         ]
    else
      Array.indexedMapToList viewPlayer players

viewPlayerInput : Int -> Player -> Bool -> Html Msg
viewPlayerInput index player canRemove =
  let
    inputId = "player-name-input-" ++ String.fromInt index
  in
  div
    [ class "material player player-input"
    --, onClick (Focus inputId)
    ]
    <| [ input
          [ id inputId
          , class "player-name-input"
          , placeholder "Player Name"
          , value player.name
          , onInput (SetPlayername index)
          , onSubmit (StartGame)
          ] []
    ] ++ if canRemove then
        [ button [ class "remove-player-button", onClick (RemovePlayer index) ]
            [ text "✕" ]  -- ✖
        ]
      else
        []

viewPlayer : Int -> Player -> Html Msg
viewPlayer index { name, score, blocked } =
  button
    [ class "material player"
    , disabled blocked
    , onClick <| if blocked then NoOp else MakeGuess index
    ]
    [ span [ class "player-name" ] [ text name ]
    , span [ class "player-score" ] [ text (String.fromInt score) ]
    ]


viewCards : Model -> Html Msg
viewCards { gameState, cards, selectedCards, cardSize, hintCards } =
  let
    viewCard_ = viewCard cardSize
  in
  div
    [ id "cards"
    , classList
        [ ( "game-preparation", gameState == Preparation)
        , ( "game-started", gameState == Started )
        , ( "game-over", gameState == Over )
        ]
    ]
    <| case gameState of
      Preparation ->
        [ viewCard_ (Card 2 "ellipse" "full" "red" 1) False False
        , viewCard_ (Card 1111 "tilde" "half" "green" 2) False False
        , viewCard_ (Card 2220 "rectangle" "empty" "blue" 3) False False
        , button [ class "material start-button", onClick StartGame ]
            [ text "Start" ]
        ]
      Started ->
        List.map
          (\c -> lazy3
            viewCard_
            c
            (List.member c.id selectedCards)
            (List.member c.id hintCards)
          )
          cards
      Over ->
        (List.map (\c -> viewCard_ c False False) cards)
        ++
        [ div [ class "game-over-screen" ]
            [ div [ class "game-over-text" ] [ text "Game Over" ]
            , button [ class "material new-game-button", onClick NewGame ]
                [ text "Play Again" ]
            ]
        ]

viewCard : (Float, Float) -> Card -> Bool -> Bool -> Html Msg
viewCard (width, height) { id, shape, pattern, color, count } selected highlighted =
  button
    [ classList
        [ ("material", True)
        , ("card", True)
        , ("card-" ++ color, True)
        , ("selected", selected)
        ]
    , style "width" <| String.fromFloat width ++ "px"
    , style "height" <| String.fromFloat height ++ "px"
    , onClick <| SelectCard id
    ]
    <|
      (List.repeat count <| lazy3 shape2svg shape pattern color)
      ++
      (if highlighted then [ FA.viewIcon FA.smileWink ] else [])


viewInfo : Model -> Html Msg
viewInfo { gameState, deck, hintCards } =
  case gameState of
    Preparation ->
      text ""
    _ ->
      div [ id "info" ]
        [ div [ class "info-text" ]
            [ text <| (List.length deck |> String.fromInt) ++ " cards left" ]
        , button
            [ class "material"
            , disabled (List.length hintCards >= 3 || gameState == Over)
            , onClick CheckImpossible
            ]
            [ text <|
                if gameState == Over then
                 "Impossible."
                else if List.length hintCards == 0 then
                  "Impossible?"
                else
                  "Possible!" ++ if List.length hintCards < 3 then " (Next Hint)" else ""
            ]
        ]


-- SVG VIEW

symbolWidth = 120
symbolHeight = 50
symbolPadding = 10

shape2svg : Shape -> Pattern -> Color -> Svg.Svg Msg
shape2svg shape pattern color =
  let
    width = symbolWidth + 2 * symbolPadding
    height = symbolHeight + 2 * symbolPadding
    viewBox = [ 0, 0, width, height ]
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
    [ SvgA.xlinkHref <| "#shape-" ++ shape  -- Svg.Attributes.href doesn't exist
    , SvgA.class <| "shape-" ++ pattern ++ " shape-" ++ color
    ] []

svgDefs : Svg.Svg Msg
svgDefs =
  Svg.svg
    [ SvgA.class "svg-defs" ]
    [ Svg.defs [] <|
        [ rectangle, tilde, ellipse ]
        ++ List.map svgPatternHatch [ "red", "green", "blue" ]
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
    ] []

diamond : Svg.Svg Msg
diamond =
  let
    (dx, dy) = (symbolWidth / 2, symbolHeight / 2)
    coordinates =
      [ symbolPadding + dx, symbolPadding, dx, dy, -dx, dy, -dx, -dy ]
    d = "m" ++ (String.join " " <| List.map String.fromFloat coordinates) ++ "z"
  in
  Svg.path
    [ SvgA.id "shape-diamond" , SvgA.class "shape" , SvgA.d d ] []

tilde : Svg.Svg Msg
tilde =
  Svg.path
    [ SvgA.id "shape-tilde"
    , SvgA.class "shape"
    , SvgA.d "m44.2 10c-25.3 0-36.4 23.7-32.4 34.6 3.9 10.7 16.5-6.3 31.1-6.8 16.4-0.6 31.1 20.3 52.9 20.3 25.3 0 36.4-23.7 32.4-34.6-3.9-10.7-16.5 6.3-31.1 6.8-16.4 0.6-31.1-20.3-52.9-20.3z"
    ] []

ellipse : Svg.Svg Msg
ellipse =
  Svg.ellipse
    [ SvgA.id "shape-ellipse"
    , SvgA.class "shape"
    , SvgA.cx <| String.fromFloat (symbolPadding + symbolWidth / 2)
    , SvgA.cy <| String.fromFloat (symbolPadding + symbolHeight / 2)
    , SvgA.rx <| String.fromFloat (symbolWidth / 2)
    , SvgA.ry <| String.fromFloat (symbolHeight / 2)
    ] []

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
        , SvgA.class <| "stroke-" ++ color
        ] []
    ]
