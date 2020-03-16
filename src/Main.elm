module Main exposing (main)

import Array exposing (Array)
import Browser
import Debug
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random
import Random.List exposing (shuffle)
import Set exposing (Set)
import Svg
import Svg.Attributes as SvgA

-- CONFIG

red = "#f44"
green = "#2b2"
blue = "#66f"

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
  }

type alias Player =
  { name : String
  , score : Int
  }

type alias Card = Int

type alias CardData =
  { shape : Shape
  , pattern : Pattern
  , color : Color
  , count : Count
  }

type Shape = Rectangle | Tilde | Ellipse
type Pattern = Full | Half | Empty
type alias Color = String
type alias Count = Int

generateDeck =
  let
    product : List a -> List (a -> b) -> List b
    product l1 = List.map (\b -> List.map b l1) >> List.concat
  in
    [CardData]
      |> product [Rectangle, Tilde, Ellipse]
      |> product [Full, Half, Empty]
      |> product [red, green, blue]
      |> product [1, 2, 3]

cardData : Card -> Maybe CardData
cardData id =
  Array.get id (Array.fromList generateDeck)


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
  (  { players = Array.fromList
      [ Player "Philipp" 0
      , Player "Susanne" 0
      ]
    , selectedPlayer = -1
    , deck = []
    , cards = []
    , selectedCards = Set.empty
    }
  , Random.generate StartGame (shuffle <| List.range 0 80)
  )


type Msg
  = StartGame (List Card)
  | SelectPlayer Int
  | SelectCard Card


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    StartGame deck ->
      ( { model |
          deck = List.drop 12 deck
        , cards =  List.take 12 deck
        }
      , Cmd.none
      )
    SelectPlayer index ->
      ( attemptGuess
          { model | selectedPlayer = index }
      , Cmd.none
      )
    SelectCard card ->
      ( attemptGuess
          { model | selectedCards =
            let
              selected = model.selectedCards
            in
              if Set.member card selected
              then Set.remove card selected
              else if Set.size selected < 3
                   then Set.insert card selected
                   else selected
          }
      , Cmd.none
      )

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

attemptGuess : Model -> Model
attemptGuess ({players, selectedPlayer, cards, deck, selectedCards} as model) =
    case Array.get selectedPlayer players of
      Nothing ->
        model
      Just player ->
        if Set.size selectedCards /= 3
        then
          model
        else
          if checkTriple selectedCards
          then
            let
              newPlayers =
                Array.set
                    selectedPlayer
                    { player | score = player.score + 3 }
                    players
              (newCards, newDeck) =
                if List.length cards > 12
                then
                  (removeSelectedCards selectedCards cards, deck)
                else
                  (replaceSelectedCards selectedCards deck cards
                  , List.drop 3 deck
                  )
            in
              { model |
                players = newPlayers
              , selectedPlayer = -1
              , cards = newCards
              , deck = newDeck
              , selectedCards = Set.empty
              }
          else
            { model |
              selectedPlayer = -1
            , selectedCards = Set.empty
            }

checkTriple : Set Card -> Bool
checkTriple selected =
  let
    cards = Set.toList selected
    validSum = \n -> Set.member n (Set.fromList [0, 3, 6])
  in
    List.all validSum
      <| List.map
          (\n -> List.sum <| List.map (\x -> modBy 3 (x // n)) cards)
          [1, 3, 9, 27]


-- VIEW

view : Model -> List (Html Msg)
view { players, selectedPlayer, cards, selectedCards } =
  [ aside [ id "sidebar" ]
    [ h1 [] [ text "SET!" ]
    , viewPlayers players selectedPlayer
    ]
  , main_ [ id "main" ]
    [ div [ id "cards" ]
      (List.map (\c -> viewCard c <| Set.member c selectedCards) cards) ]
  ]

viewPlayers : Array Player -> Int -> Html Msg
viewPlayers players selectedPlayer =
  div
    [ id "players" ]
    (List.indexedMap
      (\i p -> viewPlayer i p <| i == selectedPlayer)
      (Array.toList players))

viewPlayer : Int -> Player -> Bool -> Html Msg
viewPlayer index { name, score } selected =
  div
    [ class <| "player" ++ if selected then " selected" else ""
    , onClick <| SelectPlayer index
    ]
    [ span [ class "player-name" ] [ text name ]
    , span [ class "player-score" ] [ text (String.fromInt score) ]
    ]

viewCard : Card -> Bool -> Html Msg
viewCard card selected =
    case cardData card of
      Just {count, shape, pattern, color} ->
        div
          [ class <| "card" ++ if selected then " selected" else ""
          , onClick <| SelectCard card
          ]
          (List.repeat count <| shape2svg shape pattern color)
      Nothing ->  -- should be impossible
        div [ class "card" ] [ text "???" ]



-- SVG VIEW

shape2svg : Shape -> Pattern -> Color -> Svg.Svg Msg
shape2svg shape pattern color =
  case shape of
    Rectangle -> rectangle pattern color
    Tilde -> tilde pattern color
    Ellipse -> ellipse pattern color

wrapSvg : Svg.Svg Msg -> Svg.Svg Msg
wrapSvg child =
  Svg.svg
    [ SvgA.width <| String.fromInt symbolWidth
    , SvgA.height <| String.fromInt symbolHeight
    , SvgA.viewBox ([0, 0, symbolWidth, symbolHeight]
                    |> List.map String.fromInt |> String.join " ")
    ]
    [ child ]

rectangle : Pattern -> Color -> Svg.Svg Msg
rectangle pattern color =
  wrapSvg <| Svg.rect
    [ SvgA.x <| String.fromFloat (symbolStroke / 2)
    , SvgA.y <| String.fromFloat (symbolStroke / 2)
    , SvgA.width <| String.fromFloat (symbolWidth - 2 * symbolStroke)
    , SvgA.height <| String.fromFloat (symbolHeight - 2 * symbolStroke)
    , SvgA.style <| svgStyles pattern color
    ] []

tildePath = "m34.2 1c-25.3 0-36.4 23.7-32.4 34.6 3.9 10.7 16.5-6.3 31.1-6.8 16.4-0.6 31.1 20.3 52.9 20.3 25.3 0 36.4-23.7 32.4-34.6-3.9-10.7-16.5 6.3-31.1 6.8-16.4 0.6-31.1-20.3-52.9-20.3z"

tilde : Pattern -> Color -> Svg.Svg Msg
tilde pattern color =
  wrapSvg <| Svg.path
    [ SvgA.x <| String.fromFloat (symbolStroke / 2)
    , SvgA.y <| String.fromFloat (symbolStroke / 2)
    , SvgA.d tildePath
    , SvgA.style <| svgStyles pattern color
    ] []

ellipse : Pattern -> Color -> Svg.Svg Msg
ellipse pattern color =
  wrapSvg <| Svg.ellipse
    [ SvgA.cx <| String.fromFloat (symbolWidth / 2)
    , SvgA.cy <| String.fromFloat (symbolHeight / 2)
    , SvgA.rx <| String.fromFloat ((symbolWidth - symbolStroke) / 2)
    , SvgA.ry <| String.fromFloat ((symbolHeight - symbolStroke) / 2)
    , SvgA.style <| svgStyles pattern color
    ] []

svgStyles : Pattern -> Color -> String
svgStyles pattern color =
  String.join ";"
    [ "stroke:" ++ color
    , "stroke-width:" ++ String.fromInt symbolStroke
    , "fill:" ++ color
    , "fill-opacity:" ++ if pattern == Full then "1"
                         else if pattern == Half then "0.2" else "0"
    ]
