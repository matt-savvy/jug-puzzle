module JugSolve exposing (Steps, jugSolver)

import Jugs exposing (Jug(..), Jugs, Msg(..), applyMsg, isSolved)


type alias Steps =
    List Msg


type alias StepQueue =
    List ( Jugs, Steps )


possibleSteps : Steps
possibleSteps =
    [ Fill Gallon5
    , Fill Gallon3
    , Empty Gallon5
    , Empty Gallon3
    , Pour Gallon3 Gallon5
    , Pour Gallon5 Gallon3
    ]


jugSolver : Jugs -> Steps
jugSolver jugs =
    jugSolve jugs [] [] []


jugSolve : Jugs -> Steps -> StepQueue -> List Jugs -> Steps
jugSolve jugs steps queue seenStates =
    if isSolved jugs then
        steps

    else
        let
            nextQueue : StepQueue
            nextQueue =
                List.map (\step -> ( applyMsg step jugs, steps ++ [ step ] )) possibleSteps
                    |> List.append queue
                    |> List.filter (\item -> not (List.member (Tuple.first item) seenStates))
                    |> List.sortBy (\item -> Tuple.second item |> List.length)
        in
        case
            List.head nextQueue
        of
            Just ( nextJugs, nextSteps ) ->
                jugSolve nextJugs nextSteps nextQueue (jugs :: seenStates)

            Nothing ->
                []
