module Jugs exposing (Jug(..), JugValue, Jugs, Msg(..), Step(..), Steps, applyStep, createJug, createJugs, emptyJug, fillJug, getAvailableSteps, getCapacity, getJug, isSolved, jugSolver, main, pour, updateJug)

import Browser
import Html exposing (Html, button, div, h1, h2, li, ol, p, text)
import Html.Attributes exposing (class, classList, disabled, id)
import Html.Events exposing (onClick)


type Jug
    = Gallon3
    | Gallon5


type alias JugValue =
    ( Jug, Int )


type alias Jugs =
    ( JugValue, JugValue )


type Step
    = Fill Jug
    | Empty Jug
    | Pour Jug Jug


type Msg
    = Action Step
    | ClickedGetHint
    | ClickedUndo


type alias Steps =
    List Step


createJug : Int -> Jug -> JugValue
createJug volume jug =
    ( jug, volume )


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


pour : Jug -> Jug -> Jugs -> Jugs
pour source target jugs =
    let
        spaceLeft =
            getCapacity target - getJug target jugs

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


emptyJug : Jug -> Jugs -> Jugs
emptyJug jug jugs =
    updateJug jug 0 jugs


fillJug : Jug -> Jugs -> Jugs
fillJug jug jugs =
    updateJug jug (getCapacity jug) jugs


isSolved : Jugs -> Bool
isSolved jugs =
    getJug Gallon5 jugs == 4


type Hint
    = Hint Step
    | NoHint


type alias Model =
    { jugs : Jugs
    , steps : Steps
    , hint : Hint
    , availableSteps : Steps
    }


emptyJugs : Jugs
emptyJugs =
    createJugs 0 0


initialModel : Model
initialModel =
    { jugs = emptyJugs
    , steps = []
    , hint = NoHint
    , availableSteps = getAvailableSteps emptyJugs
    }


view : Model -> Html Msg
view model =
    div []
        [ h1 []
            [ text "measure 4 gallons exactly"
            , button [ onClick ClickedGetHint ] [ text "Get Hint" ]
            , button [ disabled (List.isEmpty model.steps), onClick ClickedUndo ] [ text "undo last move" ]
            ]
        , div [ id "jugs" ]
            [ viewJug Gallon3 "3 gallon jug" model
            , div [ id "pour-buttons" ]
                [ viewPourButton Gallon3 Gallon5 model " >> "
                , viewPourButton Gallon5 Gallon3 model " << "
                ]
            , viewJug Gallon5 "5 gallon jug" model
            ]
        , viewSteps model.steps
        ]


viewJug : Jug -> String -> Model -> Html Msg
viewJug jug jugLabel { jugs, hint, availableSteps } =
    div [ class "jug" ]
        [ h2 [] [ text (jugLabel ++ ": " ++ String.fromInt (getJug jug jugs)) ]
        , button [ disabled (isDisabled (Fill jug) availableSteps), classList [ ( "hint", hint == Hint (Fill jug) ) ], onClick (Action (Fill jug)) ] [ text "fill" ]
        , button [ disabled (isDisabled (Empty jug) availableSteps), classList [ ( "hint", hint == Hint (Empty jug) ) ], onClick (Action (Empty jug)) ] [ text "empty" ]
        ]


viewPourButton : Jug -> Jug -> Model -> String -> Html Msg
viewPourButton source target { hint, availableSteps } description =
    let
        showHint : Bool
        showHint =
            hint == Hint (Pour source target)
    in
    div [] [ button [ disabled (isDisabled (Pour source target) availableSteps), classList [ ( "hint", showHint ) ], onClick (Action (Pour source target)) ] [ text description ] ]


isDisabled : Step -> Steps -> Bool
isDisabled step availableSteps =
    not (List.member step availableSteps)


viewSteps : Steps -> Html Msg
viewSteps steps =
    div [] [ ol [] (List.map viewStep steps) ]


stepToString : Step -> String
stepToString step =
    case step of
        Fill Gallon3 ->
            "Fill 3 gallon"

        Empty Gallon3 ->
            "Empty 3 gallon"

        Pour Gallon3 Gallon5 ->
            "Pour 3 gallon into 5 gallon"

        Fill Gallon5 ->
            "Fill 5 gallon"

        Empty Gallon5 ->
            "Empty 5 gallon"

        Pour Gallon5 Gallon3 ->
            "Pour 5 gallon into 3 gallon"

        -- invalid states for completeness
        Pour Gallon5 Gallon5 ->
            "Pour 5 gallon into 5 gallon"

        Pour Gallon3 Gallon3 ->
            "Pour 3 gallon into 3 gallon"


viewStep : Step -> Html Msg
viewStep step =
    li [] [ text (stepToString step) ]


applyStep : Step -> Jugs -> Jugs
applyStep step jugs =
    case step of
        Fill jug ->
            fillJug jug jugs

        Empty jug ->
            emptyJug jug jugs

        Pour source target ->
            pour source target jugs


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedGetHint ->
            let
                nextStep : Maybe Step
                nextStep =
                    List.head (jugSolver model.jugs)
            in
            case nextStep of
                Just step ->
                    let
                        stepsWithHint : Steps
                        stepsWithHint =
                            model.steps ++ [ step ]

                        jugsWithHint : Jugs
                        jugsWithHint =
                            applyStep step model.jugs
                    in
                    ( { model | hint = Hint step }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        Action step ->
            let
                nextJugs : Jugs
                nextJugs =
                    applyStep step model.jugs
            in
            if nextJugs == model.jugs then
                ( model, Cmd.none )

            else
                ( { model | steps = model.steps ++ [ step ], jugs = nextJugs, hint = NoHint, availableSteps = getAvailableSteps nextJugs }, Cmd.none )

        ClickedUndo ->
            let
                prevSteps =
                    List.take (List.length model.steps - 1) model.steps

                prevJugs =
                    List.foldl applyStep emptyJugs prevSteps
            in
            ( { model | steps = prevSteps, jugs = prevJugs, hint = NoHint, availableSteps = getAvailableSteps prevJugs }, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


type alias StepQueue =
    List ( Jugs, Steps )


possibleSteps : Steps
possibleSteps =
    [ Fill Gallon3
    , Fill Gallon5
    , Empty Gallon3
    , Empty Gallon5
    , Pour Gallon3 Gallon5
    , Pour Gallon5 Gallon3
    ]


getAvailableSteps : Jugs -> Steps
getAvailableSteps jugs =
    List.filter (\step -> applyStep step jugs /= jugs) possibleSteps


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
