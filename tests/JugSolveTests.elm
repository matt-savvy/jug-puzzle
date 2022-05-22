module JugSolveTests exposing (..)

import Expect exposing (Expectation)
import Jugs exposing (..)
import Test exposing (..)


type alias Steps =
    List Msg


possibleSteps : Steps
possibleSteps =
    [ Fill Gallon5
    , Fill Gallon3
    , Empty Gallon5
    , Empty Gallon3
    , Pour Gallon3 Gallon5
    , Pour Gallon5 Gallon3
    ]


isSolved : Jugs -> Bool
isSolved jugs =
    getJug Gallon5 jugs == 4


jugSolver : Jugs -> Steps
jugSolver jugs =
    jugSolve jugs [] [] 0


jugSolve : Jugs -> Steps -> List Jugs -> Int -> Steps
jugSolve jugs steps seenStates stepCounter =
    if isSolved jugs then
        steps

    else if stepCounter > 20 then
        -- prevent infinte loops
        []

    else
        let
            -- filter out all steps which will give us the same state
            availableSteps : Steps
            availableSteps =
                List.filter (\step -> applyMsg step jugs /= jugs) possibleSteps
                    -- filter out all steps which will give us a step we've seen before
                    |> List.filter (\step -> not (List.member (applyMsg step jugs) seenStates))
        in
        case
            List.head
                (List.map (\step -> jugSolve (applyMsg step jugs) (List.append steps [ step ]) (List.append seenStates [ jugs ]) (stepCounter + 1)) availableSteps)
        of
            Just head ->
                head

            Nothing ->
                []


solution : Steps
solution =
    [ Fill Gallon5
    , Pour Gallon5 Gallon3
    , Empty Gallon3
    , Pour Gallon5 Gallon3
    , Fill Gallon5
    , Pour Gallon5 Gallon3
    ]


jugSolverTest : Test
jugSolverTest =
    test "sanity" <| \_ -> Expect.equal (jugSolver (createJugs 0 0)) solution


isSolvedTest : Test
isSolvedTest =
    describe "is solved tests"
        [ test "3, 5" <| \_ -> Expect.equal (isSolved (createJugs 3 5)) False
        , test "3, 4" <| \_ -> Expect.equal (isSolved (createJugs 3 4)) True
        , test "2, 2" <| \_ -> Expect.equal (isSolved (createJugs 2 2)) False
        ]
