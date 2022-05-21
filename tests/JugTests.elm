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


updateJugTest : String -> Jug -> Int -> Jugs -> Jugs -> Test
updateJugTest description jug newValue jugs expectedJugs =
    test description <|
        \_ -> Expect.equal (updateJug jug newValue jugs) expectedJugs


updateJugTests : Test
updateJugTests =
    describe "test updateJug"
        [ updateJugTest "update Gallon3 should update Gallon3" Gallon3 2 initJugs ( ( Gallon3, 2 ), ( Gallon5, 5 ) )
        , updateJugTest "update Gallon5 should update Gallon5" Gallon5 4 initJugs ( ( Gallon3, 3 ), ( Gallon5, 4 ) )
        ]
