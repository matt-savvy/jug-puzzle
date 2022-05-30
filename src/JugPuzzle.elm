module JugPuzzle exposing (Model, Msg, main)

import Browser
import Html exposing (Html, button, div, h1, h2, li, ol, span, text)
import Html.Attributes exposing (class, classList, disabled, id, style)
import Html.Events exposing (onClick)
import Jugs exposing (Hint(..), Jug(..), Jugs, Step(..), Steps(..), applyStep, applySteps, createJugs, dropLastStep, emptySteps, getAvailableSteps, getCapacity, getHint, getJug, isSolved, pushStep, stepMap, stepMember)
import Task
import Time exposing (Posix, posixToMillis)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { jugs : Jugs
    , steps : Steps
    , hint : Hint
    , availableSteps : List Step
    , startTime : Maybe Posix
    , currentTime : Maybe Posix
    }


emptyJugs : Jugs
emptyJugs =
    createJugs 0 0


initialModel : Model
initialModel =
    { jugs = emptyJugs
    , steps = emptySteps
    , hint = NoHint
    , availableSteps = getAvailableSteps emptyJugs
    , startTime = Nothing
    , currentTime = Nothing
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Cmd.batch
        [ Task.perform GotStartTime Time.now
        , Task.perform GotCurrentTime Time.now
        ]
    )



-- UPDATE


type Msg
    = Action Step
    | ClickedGetHint
    | ClickedUndo
    | ClickedReset
    | GotStartTime Posix
    | GotCurrentTime Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotStartTime time ->
            ( { model | startTime = Just time }, Cmd.none )

        GotCurrentTime time ->
            ( { model | currentTime = Just time }, Cmd.none )

        ClickedGetHint ->
            ( { model | hint = getHint model.jugs }, Cmd.none )

        Action step ->
            let
                nextJugs : Jugs
                nextJugs =
                    applyStep step model.jugs

                cmd : Cmd Msg
                cmd =
                    if isSolved nextJugs then
                        Task.perform GotCurrentTime Time.now

                    else
                        Cmd.none
            in
            if nextJugs == model.jugs then
                ( model, cmd )

            else
                ( { model | steps = pushStep model.steps step, jugs = nextJugs, hint = NoHint, availableSteps = getAvailableSteps nextJugs }, cmd )

        ClickedUndo ->
            let
                prevSteps : Steps
                prevSteps =
                    dropLastStep model.steps

                prevJugs : Jugs
                prevJugs =
                    applySteps emptyJugs prevSteps
            in
            ( { model | steps = prevSteps, jugs = prevJugs, hint = NoHint, availableSteps = getAvailableSteps prevJugs }, Cmd.none )

        ClickedReset ->
            init ()



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions { jugs } =
    if isSolved jugs then
        Sub.none

    else
        Time.every 1000 GotCurrentTime



-- VIEW


view : Model -> Html Msg
view model =
    let
        noStepsMade : Bool
        noStepsMade =
            model.steps == emptySteps

        solved : Bool
        solved =
            isSolved model.jugs

        message : String
        message =
            if solved then
                "You did it!"

            else
                "Fill one of the jugs with exactly four gallons of water"
    in
    div []
        [ div [ id "time-container" ] [ viewTime model ]
        , h1 [] [ text message ]
        , div [ id "game-buttons" ]
            [ button [ disabled noStepsMade, onClick ClickedReset ] [ text "reset" ]
            , button [ disabled solved, onClick ClickedGetHint ] [ text "get hint" ]
            , button [ disabled (solved || noStepsMade), onClick ClickedUndo ] [ text "undo" ]
            ]
        , div [ id "jugs" ]
            [ viewJug Gallon3 "3 gallon jug" model solved
            , div [ id "pour-buttons" ]
                [ viewPourButton Gallon3 Gallon5 model solved " >> "
                , viewPourButton Gallon5 Gallon3 model solved " << "
                ]
            , viewJug Gallon5 "5 gallon jug" model solved
            ]
        , viewSteps model.steps
        ]


getTimeDelta : Posix -> Posix -> Int
getTimeDelta timeA timeB =
    abs (posixToMillis timeA - posixToMillis timeB)


getMinutesSeconds : Int -> ( Int, Int )
getMinutesSeconds delta =
    let
        deltaSeconds : Int
        deltaSeconds =
            delta // 1000

        minutes : Int
        minutes =
            deltaSeconds // 60

        seconds : Int
        seconds =
            modBy 60 deltaSeconds
    in
    ( minutes, seconds )


humanizeMinutesSeconds : ( Int, Int ) -> String
humanizeMinutesSeconds ( mins, secs ) =
    let
        padZeroes : String -> String
        padZeroes str =
            String.padLeft 2 '0' str

        convert : Int -> String
        convert x =
            String.fromInt x |> padZeroes
    in
    convert mins ++ ":" ++ convert secs


viewTime : Model -> Html Msg
viewTime { startTime, currentTime } =
    case ( startTime, currentTime ) of
        ( Just start, Just current ) ->
            let
                elapsedTime : String
                elapsedTime =
                    getTimeDelta start current |> getMinutesSeconds |> humanizeMinutesSeconds
            in
            text elapsedTime

        _ ->
            text ""


getId : Jug -> String
getId jug =
    case jug of
        Gallon3 ->
            "gallon-3"

        Gallon5 ->
            "gallon-5"


getFill : Jug -> Jugs -> String
getFill jug jugs =
    let
        filledPercent : Float
        filledPercent =
            100 * toFloat (getJug jug jugs) / toFloat (getCapacity jug)

        filled : String
        filled =
            String.fromFloat filledPercent
    in
    "linear-gradient(to top, aqua 0% " ++ filled ++ "%, white " ++ filled ++ "%)"


viewJug : Jug -> String -> Model -> Bool -> Html Msg
viewJug jug jugLabel { jugs, hint, availableSteps } solved =
    div [ class "jug-container" ]
        [ h2 [] [ text (jugLabel) ]
        , div [ class "jug-inner" ]
            [ div [ class "jug", id (getId jug), style "background" (getFill jug jugs) ] []
            , button [ disabled (solved || isNoOp (Fill jug) availableSteps), classList [ ( "hint", hint == Hint (Fill jug) ) ], onClick (Action (Fill jug)) ] [ text "fill" ]
            , button [ disabled (solved || isNoOp (Empty jug) availableSteps), classList [ ( "hint", hint == Hint (Empty jug) ) ], onClick (Action (Empty jug)) ] [ text "empty" ]
            ]
        ]


viewPourButton : Jug -> Jug -> Model -> Bool -> String -> Html Msg
viewPourButton source target { hint, availableSteps } solved description =
    let
        showHint : Bool
        showHint =
            hint == Hint (Pour source target)
    in
    div [] [ button [ disabled (solved || isNoOp (Pour source target) availableSteps), classList [ ( "hint", showHint ) ], onClick (Action (Pour source target)) ] [ text description ] ]


isNoOp : Step -> List Step -> Bool
isNoOp step availableSteps =
    not (List.member step availableSteps)


viewSteps : Steps -> Html Msg
viewSteps steps =
    div [] [ ol [] (stepMap viewStep steps) ]


viewStep : Step -> Html Msg
viewStep step =
    li [] [ text (stepToString step) ]


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
