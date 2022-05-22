module JugTests exposing (..)

import Expect exposing (Expectation)
import Jugs exposing (..)
import Test exposing (..)


initJugsFull : Jugs
initJugsFull =
    ( ( Gallon3, 3 )
    , ( Gallon5, 5 )
    )


initJugsEmpty : Jugs
initJugsEmpty =
    ( ( Gallon3, 0 )
    , ( Gallon5, 0 )
    )


getJugTest : String -> Jug -> Jugs -> Int -> Test
getJugTest description jug jugs expectedValue =
    test description <|
        \_ -> Expect.equal (getJug jug jugs) expectedValue


getJugsTests : Test
getJugsTests =
    describe "test getJug"
        [ getJugTest "getJug Gallon3 should return the val for gallon 3" Gallon3 initJugsFull 3
        , getJugTest "getJug Gallon5 should return the val for gallon 5" Gallon5 initJugsFull 5
        ]


updateJugTest : String -> Jug -> Int -> Jugs -> Jugs -> Test
updateJugTest description jug newValue jugs expectedJugs =
    test description <|
        \_ -> Expect.equal (updateJug jug newValue jugs) expectedJugs


updateJugTests : Test
updateJugTests =
    describe "test updateJug"
        [ updateJugTest "update Gallon3 should update Gallon3" Gallon3 2 initJugsFull ( ( Gallon3, 2 ), ( Gallon5, 5 ) )
        , updateJugTest "update Gallon5 should update Gallon5" Gallon5 4 initJugsFull ( ( Gallon3, 3 ), ( Gallon5, 4 ) )
        ]


testGetMax3 : Test
testGetMax3 =
    test "Gallon3 should have a max of 3" <|
        \_ -> Expect.equal (getCapacity Gallon3) 3


testGetMax5 : Test
testGetMax5 =
    test "Gallon5 should have a max of 5" <|
        \_ -> Expect.equal (getCapacity Gallon5) 5


pourTests : Test
pourTests =
    describe "test pour"
        -- pouring from 3 to 5
        [ pourTest "pour Gallon3 3 -> Gallon5 0 should return 0, 3" Gallon3 Gallon5 ( ( Gallon3, 3 ), ( Gallon5, 0 ) ) ( ( Gallon3, 0 ), ( Gallon5, 3 ) )
        , pourTest "pour Gallon3 3 -> Gallon5 4 should return 2, 5" Gallon3 Gallon5 ( ( Gallon3, 3 ), ( Gallon5, 4 ) ) ( ( Gallon3, 2 ), ( Gallon5, 5 ) )
        , pourTest "pour Gallon3 3 -> Gallon5 5 should return 3, 5" Gallon3 Gallon5 ( ( Gallon3, 3 ), ( Gallon5, 5 ) ) ( ( Gallon3, 3 ), ( Gallon5, 5 ) )

        -- pouring from 5 to 3
        , pourTest "pour Gallon3 0 <- Gallon5 5 should return 3, 2" Gallon5 Gallon3 ( ( Gallon3, 0 ), ( Gallon5, 5 ) ) ( ( Gallon3, 3 ), ( Gallon5, 2 ) )
        , pourTest "pour Gallon3 2 <- Gallon5 5 should return 3, 4" Gallon5 Gallon3 ( ( Gallon3, 2 ), ( Gallon5, 5 ) ) ( ( Gallon3, 3 ), ( Gallon5, 4 ) )
        , pourTest "pour Gallon3 3 <- Gallon5 2 should return 3, 2" Gallon5 Gallon3 ( ( Gallon3, 3 ), ( Gallon5, 2 ) ) ( ( Gallon3, 3 ), ( Gallon5, 2 ) )
        ]


pourTest : String -> Jug -> Jug -> Jugs -> Jugs -> Test
pourTest description source target jugs expectedResult =
    test description <|
        \_ -> Expect.equal (pour source target jugs) expectedResult


emptyJugTests : Test
emptyJugTests =
    describe "empty jug tests"
        [ test "empty Gallon3" <| \_ -> Expect.equal (emptyJug Gallon3 initJugsFull) ( ( Gallon3, 0 ), ( Gallon5, 5 ) )
        , test "empty Gallon5" <| \_ -> Expect.equal (emptyJug Gallon5 initJugsFull) ( ( Gallon3, 3 ), ( Gallon5, 0 ) )
        ]


fillJugTests : Test
fillJugTests =
    describe "fill jug tests"
        [ test "fill Gallon3" <| \_ -> Expect.equal (fillJug Gallon3 initJugsEmpty) ( ( Gallon3, 3 ), ( Gallon5, 0 ) )
        , test "fill Gallon5" <| \_ -> Expect.equal (fillJug Gallon5 initJugsEmpty) ( ( Gallon3, 0 ), ( Gallon5, 5 ) )
        ]
