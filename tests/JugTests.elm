module JugTests exposing (..)

import Expect exposing (Expectation)
import Jugs exposing (..)
import Test exposing (..)


initJugs : Jugs
initJugs =
    ( ( Gallon3, 3 )
    , ( Gallon5, 5 )
    )




getJugTest : String -> Jug -> Jugs -> Int -> Test
getJugTest description jug jugs expectedValue =
    test description <|
        \_ -> Expect.equal (getJug jug jugs) expectedValue


getJugsTests : Test
getJugsTests =
    describe "test getJug"
        [ getJugTest "getJug Gallon3 should return the val for gallon 3" Gallon3 initJugs 3
        , getJugTest "getJug Gallon5 should return the val for gallon 5" Gallon5 initJugs 5
        ]
