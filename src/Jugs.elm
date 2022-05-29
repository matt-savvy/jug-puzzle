module Jugs exposing (Hint(..), Jug(..), JugValue, Jugs, Step(..), Steps, applyStep, createJugs, emptyJug, fillJug, getAvailableSteps, getCapacity, getHint, getJug, isSolved, jugSolver, pour, updateJug)

-- HIGH LEVEL PUZZLE LOGIC


type Hint
    = Hint Step
    | NoHint


getHint : Jugs -> Hint
getHint jugs =
    case List.head (jugSolver jugs) of
        Just step ->
            Hint step

        Nothing ->
            NoHint


getAvailableSteps : Jugs -> Steps
getAvailableSteps jugs =
    List.filter (\step -> applyStep step jugs /= jugs) possibleSteps


jugSolver : Jugs -> Steps
jugSolver jugs =
    jugSolve jugs [] [] []


type alias StepQueue =
    List ( Jugs, Steps )


jugSolve : Jugs -> Steps -> StepQueue -> List Jugs -> Steps
jugSolve jugs steps queue seenStates =
    if isSolved jugs then
        steps

    else
        let
            nextQueue : StepQueue
            nextQueue =
                List.map (\step -> ( applyStep step jugs, steps ++ [ step ] )) possibleSteps
                    |> List.append queue
                    |> List.filter (\( state, _ ) -> not (List.member state seenStates))
                    |> List.sortBy (\( _, stepList ) -> List.length stepList)
        in
        case
            List.head nextQueue
        of
            Just ( nextJugs, nextSteps ) ->
                jugSolve nextJugs nextSteps nextQueue (jugs :: seenStates)

            Nothing ->
                []


possibleSteps : Steps
possibleSteps =
    [ Fill Gallon3
    , Fill Gallon5
    , Empty Gallon3
    , Empty Gallon5
    , Pour Gallon3 Gallon5
    , Pour Gallon5 Gallon3
    ]



-- PUZZLE LOGIC


type Step
    = Fill Jug
    | Empty Jug
    | Pour Jug Jug


type alias Steps =
    List Step


applyStep : Step -> Jugs -> Jugs
applyStep step jugs =
    case step of
        Fill jug ->
            fillJug jug jugs

        Empty jug ->
            emptyJug jug jugs

        Pour source target ->
            pour source target jugs


isSolved : Jugs -> Bool
isSolved jugs =
    getJug Gallon5 jugs == 4



-- JUG BUSINESS LOGIC


emptyJug : Jug -> Jugs -> Jugs
emptyJug jug jugs =
    updateJug jug 0 jugs


fillJug : Jug -> Jugs -> Jugs
fillJug jug jugs =
    updateJug jug (getCapacity jug) jugs


pour : Jug -> Jug -> Jugs -> Jugs
pour source target jugs =
    let
        spaceLeft : Int
        spaceLeft =
            getCapacity target - getJug target jugs

        amountToPour : Int
        amountToPour =
            min spaceLeft (getJug source jugs)
    in
    case ( source, target ) of
        ( Gallon3, Gallon5 ) ->
            ( ( Gallon3, getJug Gallon3 jugs - amountToPour )
            , ( Gallon5, getJug Gallon5 jugs + amountToPour )
            )

        ( Gallon5, Gallon3 ) ->
            ( ( Gallon3, getJug Gallon3 jugs + amountToPour )
            , ( Gallon5, getJug Gallon5 jugs - amountToPour )
            )

        _ ->
            jugs



-- JUG OPERATIONS


createJugs : Int -> Int -> Jugs
createJugs volume3gallon volume5gallon =
    ( ( Gallon3, volume3gallon ), ( Gallon5, volume5gallon ) )


getJug : Jug -> Jugs -> Int
getJug jug jugs =
    case jug of
        Gallon3 ->
            Tuple.second (Tuple.first jugs)

        Gallon5 ->
            Tuple.second (Tuple.second jugs)


updateJug : Jug -> Int -> Jugs -> Jugs
updateJug jug newValue jugs =
    case jug of
        Gallon3 ->
            ( ( Gallon3, newValue ), Tuple.second jugs )

        Gallon5 ->
            ( Tuple.first jugs, ( Gallon5, newValue ) )


getCapacity : Jug -> Int
getCapacity jug =
    case jug of
        Gallon3 ->
            3

        Gallon5 ->
            5



-- JUG


type Jug
    = Gallon3
    | Gallon5


type alias JugValue =
    ( Jug, Int )


type alias Jugs =
    ( JugValue, JugValue )
