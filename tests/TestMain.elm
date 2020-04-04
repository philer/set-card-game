module TestMain exposing (..)

import Main exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Set
import Array

suiteCheckTriple : Test
suiteCheckTriple =
    describe "checkTriple"
        [ test "checkTriple [ 0, 1, 2 ] == True" <|
            \_ -> Expect.equal True <| Main.checkTriple [ 0, 1, 2 ]
        , test "checkTriple [ 0, 11, 2 ] == False" <|
            \_ -> Expect.equal False <| Main.checkTriple [ 0, 11, 2 ]
        , test "checkTriple [ 0, 1, 10 ] == False" <|
            \_ -> Expect.equal False <| Main.checkTriple [ 0, 1, 10 ]
        , test "checkTriple [ 1, 10, 0 ] == False" <|
            \_ -> Expect.equal False <| Main.checkTriple [ 1, 10, 0 ]
        , test "checkTriple [ 1, 0, 11 ] == False" <|
            \_ -> Expect.equal False <| Main.checkTriple [ 1, 0, 11 ]
        , test "checkTriple [ 10, 11, 12 ] == True" <|
            \_ -> Expect.equal True <| Main.checkTriple [ 10, 11, 12 ]
        , test "checkTriple [ 11, 12, 10 ] == True" <|
            \_ -> Expect.equal True <| Main.checkTriple [ 11, 12, 10 ]
        , test "checkTriple [ 11, 10, 12 ] == True" <|
            \_ -> Expect.equal True <| Main.checkTriple [ 11, 10, 12 ]
        , test "checkTriple [ 0, 10, 20 ] == True" <|
            \_ -> Expect.equal True <| Main.checkTriple [ 0, 10, 20 ]
        , test "checkTriple [ 0, 100, 200 ] == True" <|
            \_ -> Expect.equal True <| Main.checkTriple [ 0, 100, 200 ]
        , test "checkTriple [ 0, 1000, 2000 ] == True" <|
            \_ -> Expect.equal True <| Main.checkTriple [ 0, 1000, 2000 ]
        , test "checkTriple [ 0, 1001, 2002 ] == True" <|
            \_ -> Expect.equal True <| Main.checkTriple [ 0, 1001, 2002 ]
        , test "checkTriple [ 1, 1001, 2001 ] == True" <|
            \_ -> Expect.equal True <| Main.checkTriple [ 1, 1001, 2001 ]
        , test "checkTriple [ 1000, 1010, 1020 ] == True" <|
            \_ -> Expect.equal True <| Main.checkTriple [ 1000, 1010, 1020 ]
        , test "checkTriple [ 1000, 1010, 1021 ] == False" <|
            \_ -> Expect.equal False <| Main.checkTriple [ 1000, 1010, 1021 ]
        , test "checkTriple [ 1000, 1011, 1020 ] == False" <|
            \_ -> Expect.equal False <| Main.checkTriple [ 1000, 1011, 1020 ]
        , test "checkTriple [ 1001, 1010, 1020 ] == False" <|
            \_ -> Expect.equal False <| Main.checkTriple [ 1001, 1010, 1020 ]
        , test "checkTriple [ 1100, 1010, 1020 ] == False" <|
            \_ -> Expect.equal False <| Main.checkTriple [ 1100, 1010, 1020 ]
        , test "checkTriple [ 1000, 1110, 1020 ] == False" <|
            \_ -> Expect.equal False <| Main.checkTriple [ 1000, 1110, 1020 ]
        , test "checkTriple [ 1000, 1110, 1120 ] == False" <|
            \_ -> Expect.equal False <| Main.checkTriple [ 1000, 1110, 1120 ]
        ]

suiteGenerateDeck : Test
suiteGenerateDeck =
    describe "generateDeck"
        [ test "generateDeck length" <|
            \_ -> Expect.equal 81 (List.length generateDeck)
        , test "generateDeck maxId" <|
            \_ -> Expect.equal
                (List.minimum <| List.map .id generateDeck)
                (Just 0)
        , test "generateDeck minId" <|
            \_ -> Expect.equal
                (List.maximum <| List.map .id generateDeck)
                (Just 2222)
        , test "generateDeck has all shapes" <|
            \_ -> Expect.equal
                (Set.fromList <| List.map .shape generateDeck)
                (Set.fromList <| Array.toList <| Main.shapes)
        , test "generateDeck has all patterns" <|
            \_ -> Expect.equal
                (Set.fromList <| List.map .pattern generateDeck)
                (Set.fromList <| Array.toList <| Main.patterns)
        , test "generateDeck has all colors" <|
            \_ -> Expect.equal
                (Set.fromList <| List.map .color generateDeck)
                (Set.fromList <| Array.toList <| Main.colors)
        , test "generateDeck has all counts" <|
            \_ -> Expect.equal
                (Set.fromList <| List.map .count generateDeck)
                (Set.fromList <| Array.toList <| Main.counts)
        ]

suiteDistinctCombinations : Test
suiteDistinctCombinations =
    describe "distinctCombinations"
        [ test "distinctCombinations 2 [ 1, 2, 3 ]" <|
            \_ ->
                Expect.equalSets
                    (Set.fromList <| distinctCombinations 2 [ 1, 2, 3 ])
                    (Set.fromList <| [ [1, 2], [1, 3], [2, 3] ])
        , test "distinctCombinations 0 [ 1, 2, 3, 4 ]" <|
            \_ ->
                Expect.equalSets
                    (Set.fromList <| distinctCombinations 0 [ 1, 2, 3, 4 ])
                    (Set.fromList <| [ ])
        , test "distinctCombinations 1 [ 1, 2, 3, 4 ]" <|
            \_ ->
                Expect.equalSets
                    (Set.fromList <| distinctCombinations 1 [ 1, 2, 3, 4 ])
                    (Set.fromList <| [ [1], [2], [3], [4] ])
        , test "distinctCombinations 2 [ 1, 2, 3, 4 ]" <|
            \_ ->
                Expect.equalSets
                    (Set.fromList <| distinctCombinations 2 [ 1, 2, 3, 4 ])
                    (Set.fromList <| [ [1, 2], [1, 3], [1, 4], [2, 3], [2, 4], [3, 4] ])
        , test "distinctCombinations 3 [ 1, 2, 3, 4 ]" <|
            \_ ->
                Expect.equalSets
                    (Set.fromList <| distinctCombinations 3 [ 1, 2, 3, 4 ])
                    (Set.fromList <| [ [1, 2, 3], [1, 2, 4], [1, 3, 4], [2, 3, 4] ])
        , test "distinctCombinations 4 [ 1, 2, 3, 4 ]" <|
            \_ ->
                Expect.equal
                    (distinctCombinations 4 [ 1, 2, 3, 4 ])
                    ([ [ 1, 2, 3, 4 ] ])
        ]
