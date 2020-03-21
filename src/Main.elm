module Main exposing (main)

import Array exposing (Array)
import Array.Extra as Array
import Browser
import Browser.Dom as Dom
import Browser.Events as Events
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Html.Lazy exposing (lazy, lazy2, lazy3)
import List.Extra as List
import Random
import Random.List exposing (shuffle)
import Set exposing (Set)
import Svg
import Svg.Attributes as SvgA
import Task exposing (Task)

-- MODEL

type alias Model =
  { gameState : GameState
  , players : Array Player
  , selectedPlayer : Int
  , deck : List Card
  , cards : List Card
  , selectedCards : Set Card
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

type alias Card = Int

type alias CardData =
  { shape : Shape
  , pattern : Pattern
  , color : Color
  , count : Count
  }

type alias Shape = String
type alias Pattern = String
type alias Color = String
type alias Count = Int

generateDeck : Array CardData
generateDeck =
  let
    product : List a -> List (a -> b) -> List b
    product l1 = List.map (\b -> List.map b l1) >> List.concat
  in
  [CardData]
    |> product ["rectangle", "tilde", "ellipse"]
    |> product ["full", "half", "empty"]
    |> product ["red", "green", "blue"]
    |> product [1, 2, 3]
    |> Array.fromList

cardData : Card -> Result String CardData
cardData id =
  Array.get id generateDeck |> Result.fromMaybe "invalid card"


-- MAIN

main : Program () Model Msg
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

init : flags -> ( Model, Cmd Msg )
init _ =
  ( { gameState = Preparation
    , players = Array.fromList [ newPlayer "Player 1", newPlayer "Player 2" ]
    , selectedPlayer = -1
    , deck = []
    , cards = []
    , selectedCards = Set.empty
    , validTriple = Nothing
    , cardSize = (200, 300)  -- arbitrary value
    }
  , updateCardSize
  )

type Msg
  = NoOp
  | WindowResize
  | SetCardSize (Result Dom.Error Dom.Element)
  | AddPlayer
  | RemovePlayer Int
  | SetPlayername Int String
  | StartGame
  | SetDeck (List Card)
  | AddCards
  | SelectPlayer Int
  | SelectCard Card

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
          [ Random.generate SetDeck (shuffle <| List.range 0 80)
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
    AddCards ->
      ( unblockAllPlayers
          <| case findValidTriple cards of
            Nothing ->
              { model
              | deck = List.drop 3 deck
              , cards = cards ++ List.take 3 deck
              }
            validTriple ->
              { model | validTriple = validTriple }
      , Cmd.none
      )
    SelectPlayer index ->
      ( attemptGuess
          { model | selectedPlayer = index }
      , Cmd.none
      )
    SelectCard card ->
      ( attemptGuess
          { model |
            selectedCards =
              if Set.member card selectedCards then
                Set.remove card selectedCards
              else if Set.size selectedCards < 3 then
                Set.insert card selectedCards
              else
                selectedCards
          }
      , Cmd.none
      )

attemptGuess : Model -> Model
attemptGuess ({players, selectedPlayer, cards, deck, selectedCards} as model) =
    case Array.get selectedPlayer players of
      Nothing ->
        model
      Just player ->
        if Set.size selectedCards /= 3 then
          model
        else if checkTriple (Set.toList selectedCards) then
          let
            (newDeck, newCards) =
              if List.length cards <= 12 && List.length deck > 0 then
                ( List.drop 3 deck
                , replaceSelectedCards selectedCards deck cards
                )
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
            , selectedPlayer = -1
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
            , selectedPlayer = -1
            , selectedCards = Set.empty
            }

checkTriple : List Card -> Bool
checkTriple cards =
  let
    validSum = \n -> Set.member n (Set.fromList [0, 3, 6])
  in
  List.all validSum
    <| List.map
        (\n -> List.sum <| List.map (\x -> modBy 3 (x // n)) cards)
        [1, 3, 9, 27]

removeSelectedCards : Set Card -> List Card -> List Card
removeSelectedCards selected =
  List.filter (\card -> not <| Set.member card selected)

replaceSelectedCards : Set Card -> List Card -> List Card -> List Card
replaceSelectedCards selected deck =
  let
    zip : List a -> List a -> List (a, a)
    zip = List.map2 Tuple.pair

    replacements : Dict Card Card
    replacements = Dict.fromList <| zip (Set.toList selected) deck

    replace : Card -> Card
    replace card =
      case Dict.get card replacements of
        Just newCard -> newCard
        Nothing -> card
  in
  List.map replace

unblockAllPlayers : Model -> Model
unblockAllPlayers model =
  { model | players = Array.map (\p -> { p | blocked = False }) model.players }

checkBlockedPlayers : Model -> Model
checkBlockedPlayers ({ players } as model) =
  if List.all .blocked (Array.toList players) then
    unblockAllPlayers model
  else
    model

findValidTriple : List Card -> Maybe (List Card)
findValidTriple cards =
  List.foldl
    (\triple result ->
      if result == Nothing && Set.size (Set.fromList triple) == 3
                           && checkTriple triple then
        Just triple
      else
        result
    )
    Nothing
    -- TODO optimization: lazy product without duplicates
    (List.cartesianProduct [ cards, cards, cards ])


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
viewPlayers { gameState, players, selectedPlayer } =
  div [ id "players" ]
    <| Array.toList
      <| if gameState == Preparation then
        let
          canRemove = Array.length players > 1
        in
        Array.indexedMap (\i p -> viewPlayerInput i p canRemove) players
        |> (Array.push <| button
            [ class "material add-player-button"
            , onClick AddPlayer
            ]
            [ text "+" ])
      else
        Array.indexedMap (\i p -> viewPlayer i p <| i == selectedPlayer) players

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

viewPlayer : Int -> Player -> Bool -> Html Msg
viewPlayer index { name, score, blocked } selected =
  button
    [ classList
        [ ("material", True)
        , ("player", True)
        , ("selected", selected)
        ]
    , disabled blocked
    , onClick <| if blocked then NoOp else SelectPlayer index
    ]
    [ span [ class "player-name" ] [ text name ]
    , span [ class "player-score" ] [ text (String.fromInt score) ]
    ]


viewCards : Model -> Html Msg
viewCards { gameState, cards, selectedCards, cardSize } =
  div
    [ id "cards"
    , classList
        [ ( "game-preparation", gameState == Preparation)
        , ( "game-started", gameState == Started )
        , ( "game-over", gameState == Over )
        ]
    ]
    <| if gameState == Preparation then
        [ viewCard 14 False cardSize
        , viewCard 52 False cardSize
        , viewCard 54 False cardSize
        , button
            [ class "material", id "start-button", onClick StartGame ]
            [ text "Start"]
        ]
      else
        List.map
          (\c -> lazy3 viewCard c (Set.member c selectedCards) cardSize)
          cards

viewCard : Card -> Bool -> (Float, Float) -> Html Msg
viewCard card selected (cardWidth, cardHeight) =
    case cardData card of
      Ok {count, shape, pattern, color} ->
        button
          [ classList
              [ ("material", True)
              , ("card", True)
              , ("card-" ++ color, True)
              , ("selected", selected)
              ]
          , style "width" <| String.fromFloat cardWidth ++ "px"
          , style "height" <| String.fromFloat cardHeight ++ "px"
          , onClick <| SelectCard card
          ]
          (List.repeat count <| lazy3 shape2svg shape pattern color)
      Err error ->
        div [ class "card" ] [ text <| "Error: " ++ error ]


viewInfo : Model -> Html Msg
viewInfo { gameState, deck, validTriple } =
  case gameState of
    Preparation ->
      text ""
    _ ->
      div [ id "info" ]
        [ text <| (List.length deck |> String.fromInt) ++ " cards left"
        , button
            [ class "material"
            , disabled (validTriple /= Nothing)
            , onClick AddCards
            ]
            [ text <| if validTriple == Nothing then "Impossible?" else "Possible!" ]
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

tilde : Svg.Svg Msg
tilde =
  Svg.path
    [ SvgA.id "shape-tilde"
    , SvgA.class "shape"
    , SvgA.x <| String.fromFloat symbolPadding
    , SvgA.y <| String.fromFloat symbolPadding
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
