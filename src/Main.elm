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
import Json.Encode as JE exposing (encode, array, string)
import Random
import Random.List exposing (shuffle)
import Set exposing (Set)
import Svg
import Svg.Attributes as SvgA
import Task exposing (Task)

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
  , selectedCards : Set CardId
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


-- MAIN

port consoleLog : JE.Value -> Cmd msg
port storePlayerNames : String -> Cmd msg

main : Program (Maybe String) Model Msg
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

init : (Maybe String) -> ( Model, Cmd Msg )
init flags =
  let
    playerNames =
      flags
        |> Maybe.andThen (JD.decodeString (JD.array JD.string) >> Result.toMaybe)
        |> Maybe.withDefault (Array.fromList [ "Player 1", "Player 2" ])
  in
  ( { gameState = Preparation
    , players = Array.map newPlayer playerNames
    , deck = []
    , cards = []
    , selectedCards = Set.empty
    , validTriple = Nothing
    , cardSize = (200, 300)  -- arbitrary value
    }
  , Cmd.batch
      [ consoleLog (JE.string banner)
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
  | SelectPlayer Int
  | SelectCard CardId

update : Msg -> Model -> (Model, Cmd Msg)
update msg ({players, cards, deck, selectedCards} as model) =
  case msg of
    NoOp ->
      ( model, Cmd.none )
    WindowResize ->
      ( model, updateCardSize )
    SetCardSize (Err error) ->
      ( model, Cmd.none )
    SetCardSize (Ok { element }) ->
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
    NewGame ->
      ( { model
        | gameState = Preparation
        , players = Array.map (.name >> newPlayer) players
        , selectedCards = Set.empty
        , validTriple = Nothing
        }
      , Cmd.none
      )
    AddPlayer ->
      let
        name = "Player " ++ (String.fromInt <| Array.length players + 1)
      in
      ( { model | players = Array.push (newPlayer name) players }
      , updateCardSize
      )
    RemovePlayer index ->
      ( { model | players = Array.removeAt index players }
      , updateCardSize
      )
    SetPlayername index name ->
      case Array.get index players of
        Just player ->
          ( { model
            | players = Array.set index { player | name = name } players
            }
          , Cmd.none
          )
        Nothing ->
          ( model, Cmd.none )
    StartGame ->
      ( { model | gameState = Started }
      , Cmd.batch
          [ storePlayerNames
              (JE.encode 0 <| JE.array (.name >> JE.string) players)
          , Random.generate SetDeck (shuffle generateDeck)
          , updateCardSize
          ]
      )
    SetDeck newDeck ->
      ( { model
        | deck = List.drop 12 newDeck
        , cards = List.take 12 newDeck
        }
      , Cmd.none
      )
    CheckImpossible ->
      ( unblockAllPlayers
          <| case findValidTriple cards of
            Nothing ->
              if List.length deck == 0 then
                { model | gameState = Over }
              else
                { model
                | deck = List.drop 3 deck
                , cards = cards ++ List.take 3 deck
                }
            validTriple ->
              { model | validTriple = validTriple }
      , Cmd.none
      )
    SelectCard cardId ->
      ( { model
        | selectedCards =
            if Set.member cardId selectedCards then
              Set.remove cardId selectedCards
            else if Set.size selectedCards < 3 then
              Set.insert cardId selectedCards
            else
              selectedCards
        }
      , Cmd.none
      )
    SelectPlayer index ->
      ( if Set.size selectedCards == 3 then makeGuess index model else model
      , Cmd.none
      )

makeGuess : Int -> Model -> Model
makeGuess selectedPlayer ({players, cards, deck, selectedCards} as model) =
  case Array.get selectedPlayer players of
    Nothing ->  -- error, should not be possible
      model
    Just player ->
      if checkTriple (Set.toList selectedCards) then
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
                selectedPlayer
                { player | score = player.score + 3 }
                players
          , deck = newDeck
          , cards = newCards
          , selectedCards = Set.empty
          , validTriple = Nothing
          }
      else
        checkBlockedPlayers
          { model
          | players =
              Array.set
                selectedPlayer
                { player | blocked = True }
                players
          , selectedCards = Set.empty
          }

removeSelectedCards : Set CardId -> List Card -> List Card
removeSelectedCards selected =
  List.filter (\card -> not <| Set.member card.id selected)

replaceSelectedCards : List Card -> Set CardId -> List Card -> (List Card, List Card)
replaceSelectedCards deck selected =
  let
    replace : Card -> (List Card, List Card) -> (List Card, List Card)
    replace card ( newDeck, newCards ) =
      if Set.member card.id selected then
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
    , onClick <| if blocked then NoOp else SelectPlayer index
    ]
    [ span [ class "player-name" ] [ text name ]
    , span [ class "player-score" ] [ text (String.fromInt score) ]
    ]


viewCards : Model -> Html Msg
viewCards { gameState, cards, selectedCards, cardSize, validTriple } =
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
            [ text "Start"]
        ]
      Started ->
        let
          hintCard =
            case validTriple of
              Just (_::second::_) -> Just second
              _ -> Nothing
        in
        List.map
          (\c -> lazy3 viewCard_ c (Set.member c.id selectedCards) (hintCard == Just c))
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
      (if highlighted then [FA.viewIcon FA.smileWink] else [])


viewInfo : Model -> Html Msg
viewInfo { gameState, deck, validTriple } =
  case gameState of
    Preparation ->
      text ""
    _ ->
      div [ id "info" ]
        [ div [ class "info-text" ]
            [ text <| (List.length deck |> String.fromInt) ++ " cards left" ]
        , button
            [ class "material"
            , disabled (validTriple /= Nothing || gameState == Over)
            , onClick CheckImpossible
            ]
            [ text <|
                if gameState == Over then
                 "Impossible."
                else if validTriple == Nothing then
                  "Impossible?"
                else
                  "Possible!"
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
