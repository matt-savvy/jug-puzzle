module JugPuzzle exposing (Model, Msg, main)

import Browser
import Html exposing (Html, button, div, h1, h2, li, ol, span, text)
import Html.Attributes exposing (class, classList, disabled, id, style)
import Html.Events exposing (onClick)
import Jugs exposing (Hint(..), Jug(..), Jugs, Step(..), Steps(..), applyStep, applySteps, createJugs, dropLastStep, emptySteps, getAvailableSteps, getCapacity, getHint, getJug, isSolved, pushStep, stepMap, stepMember)
import Task
import Time exposing (Posix, millisToPosix, posixToMillis)



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


type Status
    = Active
    | Solved
    | GameOver


type alias Model =
    { jugs : Jugs
    , steps : Steps
    , hint : Hint
    , availableSteps : List Step
    , deadline : Maybe Posix
    , currentTime : Maybe Posix
    , gameStatus : Status
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
    , deadline = Nothing
    , currentTime = Nothing
    , gameStatus = Active
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Cmd.batch
        [ Task.perform GotCurrentTime Time.now
        , Task.perform GotStartTime Time.now
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


timeLimit : Int
timeLimit =
    60 * 1000


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotStartTime time ->
            let
                deadline : Posix
                deadline =
                    millisToPosix (posixToMillis time + timeLimit)
            in
            ( { model | deadline = Just deadline }, Cmd.none )

        GotCurrentTime currentTime ->
            case ( model.gameStatus, model.deadline ) of
                ( Active, Just deadline ) ->
                    let
                        nextGameStatus : Status
                        nextGameStatus =
                            if getTimeDelta deadline currentTime < 0 then
                                GameOver

                            else
                                model.gameStatus
                    in
                    ( { model | currentTime = Just currentTime, gameStatus = nextGameStatus }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ClickedGetHint ->
            ( { model | hint = getHint model.jugs }, Cmd.none )

        Action step ->
            let
                nextJugs : Jugs
                nextJugs =
                    applyStep step model.jugs

                nextGameStatus : Status
                nextGameStatus =
                    if isSolved nextJugs then
                        Solved

                    else
                        model.gameStatus
            in
            if nextJugs == model.jugs then
                ( model, Cmd.none )

            else
                ( { model | steps = pushStep model.steps step, jugs = nextJugs, hint = NoHint, availableSteps = getAvailableSteps nextJugs, gameStatus = nextGameStatus }, Cmd.none )

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
subscriptions { gameStatus } =
    case gameStatus of
        Active ->
            Time.every 1000 GotCurrentTime

        GameOver ->
            Sub.none

        Solved ->
            Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        noStepsMade : Bool
        noStepsMade =
            model.steps == emptySteps

        notActive : Bool
        notActive =
            model.gameStatus /= Active

        message : String
        message =
            case model.gameStatus of
                Solved ->
                    "You did it!"

                Active ->
                    "Fill one of the jugs with exactly four gallons of water"

                GameOver ->
                    "You died, game over"
    in
    div [id "game" ]
        [ div [ id "game-buttons" ]
            [ button [ disabled (noStepsMade && model.gameStatus == Active), onClick ClickedReset ] [ text "reset" ]
            , button [ disabled notActive, onClick ClickedGetHint ] [ text "get hint" ]
            , button [ disabled (notActive || noStepsMade), onClick ClickedUndo ] [ text "undo" ]
            ]
        , viewTimer model
        , h1 [] [ text message ]
        , div [ id "jugs" ]
            [ viewJug Gallon3 "3 gallon jug" model notActive
            , div [ id "pour-buttons" ]
                [ viewPourButton Gallon3 Gallon5 model notActive " >> "
                , viewPourButton Gallon5 Gallon3 model notActive " << "
                ]
            , viewJug Gallon5 "5 gallon jug" model notActive
            ]
        , viewSteps model.steps
        ]


getTimeDelta : Posix -> Posix -> Int
getTimeDelta timeA timeB =
    posixToMillis timeA - posixToMillis timeB


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


viewTimer : Model -> Html Msg
viewTimer { deadline, currentTime } =
    case ( deadline, currentTime ) of
        ( Just start, Just current ) ->
            let
                timeRemaining : Int
                timeRemaining =
                    getTimeDelta start current

                humanizedTime : String
                humanizedTime =
                    timeRemaining |> getMinutesSeconds |> humanizeMinutesSeconds

                urgent : Bool
                urgent =
                    timeRemaining < (10 * 1000)
            in
            div [ id "timer", classList [ ( "urgent", urgent ) ] ] [ text humanizedTime ]

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
viewJug jug jugLabel { jugs, hint, availableSteps } notActive =
    div [ class "jug-container" ]
        [ h2 [] [ text jugLabel ]
        , div [ class "jug-inner" ]
            [ div [ class "jug", id (getId jug), style "background" (getFill jug jugs) ] []
            , button [ disabled (notActive || isNoOp (Fill jug) availableSteps), classList [ ( "hint", hint == Hint (Fill jug) ) ], onClick (Action (Fill jug)) ] [ text "fill" ]
            , button [ disabled (notActive || isNoOp (Empty jug) availableSteps), classList [ ( "hint", hint == Hint (Empty jug) ) ], onClick (Action (Empty jug)) ] [ text "empty" ]
            ]
        ]


viewPourButton : Jug -> Jug -> Model -> Bool -> String -> Html Msg
viewPourButton source target { hint, availableSteps } notActive description =
    let
        showHint : Bool
        showHint =
            hint == Hint (Pour source target)
    in
    div [] [ button [ disabled (notActive || isNoOp (Pour source target) availableSteps), classList [ ( "hint", showHint ), ("pour-button", True) ], onClick (Action (Pour source target)) ] [ text description ] ]


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
