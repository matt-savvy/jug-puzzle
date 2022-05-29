module JugsTests exposing (createJugTests, emptyJugTests, fillJugTests, getAvailableStepsTests, getJugsTests, isSolvedTest, pourTests, testGetMax3, testGetMax5, updateJugTests)

import Expect
import Jugs exposing (Jug(..), Jugs(..), Step(..), Steps(..), createJugs, emptyJug, fillJug, getAvailableSteps, getCapacity, getJug, isSolved, jugSolver, pour, updateJug)
import Test exposing (Test, describe, test)


initJugsFull : Jugs
initJugsFull =
    createJugs 3 5


initJugsEmpty : Jugs
initJugsEmpty =
    createJugs 0 0


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
        [ updateJugTest "update Gallon3 should update Gallon3" Gallon3 2 initJugsFull (createJugs 2 5)
        , updateJugTest "update Gallon5 should update Gallon5" Gallon5 4 initJugsFull (createJugs 3 4)
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
        [ pourTest "pour Gallon3 3 -> Gallon5 0 should return 0, 3" Gallon3 Gallon5 (createJugs 3 0) (createJugs 0 3)
        , pourTest "pour Gallon3 3 -> Gallon5 4 should return 2, 5" Gallon3 Gallon5 (createJugs 3 4) (createJugs 2 5)
        , pourTest "pour Gallon3 3 -> Gallon5 5 should return 3, 5" Gallon3 Gallon5 (createJugs 3 5) (createJugs 3 5)

        -- pouring from 5 to 3
        , pourTest "pour Gallon3 0 <- Gallon5 5 should return 3, 2" Gallon5 Gallon3 (createJugs 0 5) (createJugs 3 2)
        , pourTest "pour Gallon3 2 <- Gallon5 5 should return 3, 4" Gallon5 Gallon3 (createJugs 2 5) (createJugs 3 4)
        , pourTest "pour Gallon3 3 <- Gallon5 2 should return 3, 2" Gallon5 Gallon3 (createJugs 3 2) (createJugs 3 2)
        ]


pourTest : String -> Jug -> Jug -> Jugs -> Jugs -> Test
pourTest description source target jugs expectedResult =
    test description <|
        \_ -> Expect.equal (pour source target jugs) expectedResult


emptyJugTests : Test
emptyJugTests =
    describe "empty jug tests"
        [ test "empty Gallon3" <| \_ -> Expect.equal (emptyJug Gallon3 initJugsFull) (createJugs 0 5)
        , test "empty Gallon5" <| \_ -> Expect.equal (emptyJug Gallon5 initJugsFull) (createJugs 3 0)
        ]


fillJugTests : Test
fillJugTests =
    describe "fill jug tests"
        [ test "fill Gallon3" <| \_ -> Expect.equal (fillJug Gallon3 initJugsEmpty) (createJugs 3 0)
        , test "fill Gallon5" <| \_ -> Expect.equal (fillJug Gallon5 initJugsEmpty) (createJugs 0 5)
        ]


createJugTest : String -> Int -> Int -> Test
createJugTest description volume3gallon volume5gallon =
    test description <| \_ -> Expect.equal (createJugs volume3gallon volume5gallon) (Jugs ( volume3gallon, volume5gallon ))


createJugTests : Test
createJugTests =
    describe "create jug tests"
        [ createJugTest "0, 0" 0 0
        , createJugTest "3, 5" 3 5
        , createJugTest "2, 4" 2 4
        ]


isSolvedTest : Test
isSolvedTest =
    describe "is solved tests"
        [ test "3, 5" <| \_ -> Expect.equal (isSolved (createJugs 3 5)) False
        , test "3, 4" <| \_ -> Expect.equal (isSolved (createJugs 3 4)) True
        , test "2, 2" <| \_ -> Expect.equal (isSolved (createJugs 2 2)) False
        ]


getAvailableStepTest : Int -> Int -> List Step -> Test
getAvailableStepTest amountGallon3 amountGallon5 expectedSteps =
    let
        description : String
        description =
            String.fromInt amountGallon3 ++ ", " ++ String.fromInt amountGallon5
    in
    test description <|
        \_ ->
            Expect.equal (getAvailableSteps (createJugs amountGallon3 amountGallon5)) expectedSteps


getAvailableStepsTests : Test
getAvailableStepsTests =
    describe "getAvailableStepsTests"
        [ getAvailableStepTest 0 0 [ Fill Gallon3, Fill Gallon5 ]
        , getAvailableStepTest 1 0 [ Fill Gallon3, Fill Gallon5, Empty Gallon3, Pour Gallon3 Gallon5 ]
        , getAvailableStepTest 3 0 [ Fill Gallon5, Empty Gallon3, Pour Gallon3 Gallon5 ]
        , getAvailableStepTest 0 1 [ Fill Gallon3, Fill Gallon5, Empty Gallon5, Pour Gallon5 Gallon3 ]
        , getAvailableStepTest 0 5 [ Fill Gallon3, Empty Gallon5, Pour Gallon5 Gallon3 ]
        , getAvailableStepTest 2 4 [ Fill Gallon3, Fill Gallon5, Empty Gallon3, Empty Gallon5, Pour Gallon3 Gallon5, Pour Gallon5 Gallon3 ]
        ]


solution : Steps
solution =
    Steps
        [ Fill Gallon5
        , Pour Gallon5 Gallon3
        , Empty Gallon3
        , Pour Gallon5 Gallon3
        , Fill Gallon5
        , Pour Gallon5 Gallon3
        ]


jugSolverTest : Test
jugSolverTest =
    test "find shortest solution" <| \_ -> Expect.equal (jugSolver (createJugs 0 0)) solution
