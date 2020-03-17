module Main exposing (main)

import Array exposing (Array)
import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Lazy exposing (lazy2, lazy3)
import List.Extra exposing (cartesianProduct)
import Random
import Random.List exposing (shuffle)
import Set exposing (Set)
import Svg
import Svg.Attributes as SvgA

-- CONFIG

symbolWidth = 120
symbolHeight = 50
symbolStroke = 2


-- MODEL

type alias Model =
  { players : Array Player
  , selectedPlayer : Int
  , deck : List Card
  , cards : List Card
  , selectedCards : Set Card
  , validTriple : Maybe (List Card)
  }

type alias Player =
  { name : String
  , score : Int
  , blocked : Bool
  }


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
    , subscriptions = \_ -> Sub.none
    }

init : flags -> ( Model, Cmd Msg )
init _ =
  update
    SetupGame
    { players = Array.fromList
      [ Player "Philipp" 0 False
      , Player "Susanne" 0 False
      ]
    , selectedPlayer = -1
    , deck = []
    , cards = []
    , selectedCards = Set.empty
    , validTriple = Nothing
    }


type Msg
  = NoOp
  | SetupGame
  | SetDeck (List Card)
  | AddCards
  | SelectPlayer Int
  | SelectCard Card


update : Msg -> Model -> (Model, Cmd Msg)
update msg ({cards, deck, selectedCards} as model) =
  case msg of
    NoOp ->
      (model, Cmd.none)
    SetupGame ->
      ( model, Random.generate SetDeck (shuffle <| List.range 0 80) )
    SetDeck freshDeck ->
      ( { model |
          deck = List.drop 12 freshDeck
        , cards = List.take 12 freshDeck
        }
      , Cmd.none
      )
    AddCards ->
      ( unblockAllPlayers
          <| case findValidTriple cards of
            Nothing ->
              { model |
                deck = List.drop 3 deck
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
            unblockAllPlayers
              { model |
                players =
                  Array.set
                    selectedPlayer
                    { player | score = player.score + 3 }
                    players
              , selectedPlayer = -1
              , deck = List.drop 3 deck
              , cards =
                  if List.length cards <= 12 && List.length deck > 0 then
                    replaceSelectedCards selectedCards deck cards
                  else
                    removeSelectedCards selectedCards cards
              , selectedCards = Set.empty
              , validTriple = Nothing
              }
        else
          checkBlockedPlayers
            { model |
                players =
                  Array.set
                    selectedPlayer
                    { player | blocked = True }
                    players
            , selectedPlayer = -1
            , selectedCards = Set.empty
            , validTriple = Nothing
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
    (cartesianProduct [ cards, cards, cards ])


-- VIEW

view : Model -> List (Html Msg)
view { players, selectedPlayer, deck, cards, selectedCards, validTriple } =
  [ aside [ id "sidebar" ]
    [ h1 [] [ text "SET!" ]
    , lazy2 viewPlayers players selectedPlayer
    , div [] [ text <| (List.length deck |> String.fromInt) ++ " cards left" ]
    , div
      [ class "button", onClick AddCards ]
      [ text <| if validTriple == Nothing then "Impossible?" else "Possible!" ]
    ]
  , main_ [ id "main" ]
    [ lazy2 viewCards cards selectedCards ]
  , svgDefs
  ]

viewPlayers : Array Player -> Int -> Html Msg
viewPlayers players selectedPlayer =
  div
    [ id "players" ]
    (List.indexedMap
      (\i p -> viewPlayer i p <| i == selectedPlayer)
      (Array.toList players))

viewPlayer : Int -> Player -> Bool -> Html Msg
viewPlayer index { name, score, blocked } selected =
  div
    [ classList
      [ ("player", True)
      , ("selected", selected)
      , ("blocked", blocked)
      ]
    , onClick <| if blocked then NoOp else SelectPlayer index
    ]
    [ span [ class "player-name" ] [ text name ]
    , span [ class "player-score" ] [ text (String.fromInt score) ]
    ]

viewCards : List Card -> Set Card -> Html Msg
viewCards cards selectedCards =
  div
    [ id "cards" ]
    (List.map (\c -> lazy2 viewCard c (Set.member c selectedCards)) cards)

viewCard : Card -> Bool -> Html Msg
viewCard card selected =
    case cardData card of
      Ok {count, shape, pattern, color} ->
        div
          [ classList [ ("card", True), ("selected", selected) ]
          , onClick <| SelectCard card
          ]
          (List.repeat count <| lazy3 shape2svg shape pattern color)
      Err error ->
        div [ class "card" ] [ text <| "Error: " ++ error ]


-- SVG VIEW

shape2svg : Shape -> Pattern -> Color -> Svg.Svg Msg
shape2svg shape pattern color =
  Svg.svg
    [ SvgA.width <| String.fromInt symbolWidth
    , SvgA.height <| String.fromInt symbolHeight
    , SvgA.viewBox ([0, 0, symbolWidth, symbolHeight]
                    |> List.map String.fromInt |> String.join " ")
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
    , SvgA.x <| String.fromFloat (symbolStroke / 2)
    , SvgA.y <| String.fromFloat (symbolStroke / 2)
    , SvgA.width <| String.fromFloat (symbolWidth - 2 * symbolStroke)
    , SvgA.height <| String.fromFloat (symbolHeight - 2 * symbolStroke)
    ] []

tilde : Svg.Svg Msg
tilde =
  Svg.path
    [ SvgA.id "shape-tilde"
    , SvgA.class "shape"
    , SvgA.x <| String.fromFloat (symbolStroke / 2)
    , SvgA.y <| String.fromFloat (symbolStroke / 2)
    , SvgA.d "m34.2 1c-25.3 0-36.4 23.7-32.4 34.6 3.9 10.7 16.5-6.3 31.1-6.8 16.4-0.6 31.1 20.3 52.9 20.3 25.3 0 36.4-23.7 32.4-34.6-3.9-10.7-16.5 6.3-31.1 6.8-16.4 0.6-31.1-20.3-52.9-20.3z"
    ] []

ellipse : Svg.Svg Msg
ellipse =
  Svg.ellipse
    [ SvgA.id "shape-ellipse"
    , SvgA.class "shape"
    , SvgA.cx <| String.fromFloat (symbolWidth / 2)
    , SvgA.cy <| String.fromFloat (symbolHeight / 2)
    , SvgA.rx <| String.fromFloat ((symbolWidth - symbolStroke) / 2)
    , SvgA.ry <| String.fromFloat ((symbolHeight - symbolStroke) / 2)
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
