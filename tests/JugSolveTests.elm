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
    jugSolve jugs [] [] []


jugSolve : Jugs -> Steps -> List ( Jugs, Steps ) -> List Jugs -> Steps
jugSolve jugs steps queue seenStates =
    if isSolved jugs then
        steps

    else
        let
            nextQueue : List ( Jugs, Steps )
            nextQueue =
                List.map (\step -> ( applyMsg step jugs, steps ++ [ step ] )) possibleSteps
                    |> List.append queue
                    |> List.filter (\item -> not (List.member (Tuple.first item) seenStates))
                    |> List.sortBy (\item -> Tuple.second item |> List.length)
        in
        case
            List.head nextQueue
        of
            Just head ->
                jugSolve (Tuple.first head) (Tuple.second head) nextQueue (jugs :: seenStates)

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
