module Main exposing (main)

import Browser
import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List.Extra exposing (cartesianProduct)
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

-- MAIN

main : Program () Model Msg
main =
  Browser.document
    { init = init
    , update = update
    , view = \model -> { title = "SET!", body = view model }
    , subscriptions = \_ -> Sub.none
    }


type alias Model =
  { players : List Player
  , selectedPlayer : Maybe Player
  , deck : List Card
  , cards : List Card
  , selectedCards : Set Card
  }

type alias Player =
  { name : String
  , score : Int
  }

type Shape = Rectangle | Tilde | Ellipse
shapes n =
  case n of
    0 -> Rectangle
    1 -> Tilde
    2 -> Ellipse

type Pattern = Full | Half | Empty
patterns n =
  case n of
    0 -> Full
    1 -> Half
    2 -> Empty

type alias Color = String
colors n =
  case n of
    0 -> red
    1 -> green
    2 -> blue

type alias Count = Int
counts n =
  case n of
    0 -> 0
    1 -> 1
    2 -> 2


type alias Card =
  { shape : Shape
  , pattern : Pattern
  , color : Color
  , count : Count
  }

int2card : Int -> Card
int2card n =
  let
    countIdx = modBy 3 n + 1
    shapeIdx = modBy 9 n |> remainderBy 3
    patternIdx = (modBy 27 n |> remainderBy 9) // 3
    colorIdx = (remainderBy 27 n) // 9
  in
    { shape = shapes shapeIdx
    , pattern = patterns patternIdx
    , color = colors colorIdx
    , count = counts countIdx
    }

--patterns = [Full, Half, Empty]
--colors = [red, green, blue]

--sortedDeck : List Card
--sortedDeck =
--    cartesianProduct
--      [ [Rectangle, Tilde, Ellipse]
--      , [Full, Half, Empty]
--      , [red, green, blue]
--      , [1, 2, 3]
--      ]

init : flags -> ( Model, Cmd Msg )
init _ =
  ( { players =
      [ Player "Philipp" 0
      , Player "Susanne" 0
      ]
    , selectedPlayer = Nothing
    , deck = []
    , cards = []
    , selectedCards = Set.empty
    }
  , Random.generate StartGame (shuffle <| List.map int2card <| List.range 0 80)
  )


type Msg
  = StartGame (List Card)
  | SelectPlayer Player
  --| SelectCard Card
  --| UpdateCheck


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    StartGame deck -> ({ model | deck = Debug.log "deck" deck }, Cmd.none)
    SelectPlayer player ->
      ( { model | selectedPlayer = Just player }
      , Cmd.none
      )
    --SelectCard card ->
    --  ( { model | selectedCards =
    --      (if Set.member card model.selectedCards then Set.remove else Set.insert)
    --      card
    --      model.selectedCards
    --    }
    --  , UpdateCheck
    --  )
    --UpdateCheck ->
    --  ( model, Cmd.none )


-- VIEW

view : Model -> List (Html Msg)
view model =
  [ aside [ id "sidebar" ]
    [ h1 [] [ text "SET!" ]
    , viewPlayers model.players model.selectedPlayer
    ]
  , main_ [id "main" ] []
  ]

viewPlayers : List Player -> Maybe Player -> Html Msg
viewPlayers players selectedPlayer =
  div
    [ id "players" ]
    (List.map (\p -> viewPlayer p <| Just p == selectedPlayer) players)
    --(List.map2 viewPlayer players (List.map ((==) selectedPlayer) (List.map Just players)))

viewPlayer : Player -> Bool -> Html Msg
viewPlayer player selected =
  div
    [ class <| "player" ++ if selected then " selected" else ""
    , onClick <| SelectPlayer player
    ]
    [ span [ class "player-name" ] [ text player.name ]
    , span [ class "player-score" ] [ text (String.fromInt player.score) ]
    ]

viewCard : Card -> Html Msg
viewCard card =
  div [ class "card" ] (List.repeat card.count (shape2svg card.shape card.pattern card.color))


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
    , SvgA.viewBox <| "0 0 " ++ (String.fromInt symbolWidth) ++ " " ++ (String.fromInt symbolHeight)
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
    , "fill-opacity:" ++ if pattern == Full then "1" else if pattern == Half then "0.2" else "0"
    ]
