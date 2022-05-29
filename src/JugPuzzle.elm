module JugPuzzle exposing (main)

import Browser
import Html exposing (Html, button, div, h1, h2, li, ol, p, text)
import Html.Attributes exposing (class, classList, disabled, id)
import Html.Events exposing (onClick)
import Jugs exposing (Hint(..), Jug(..), Jugs, Step(..), Steps, applyStep, createJugs, getAvailableSteps, getHint, getJug, jugSolver)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }



-- MODEL


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



-- UPDATE


type Msg
    = Action Step
    | ClickedGetHint
    | ClickedUndo
    | ClickedReset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedGetHint ->
            ( { model | hint = getHint model.jugs }, Cmd.none )

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

        ClickedReset ->
            ( initialModel, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 []
            [ text "measure 4 gallons exactly"
            , button [ disabled (List.isEmpty model.steps), onClick ClickedReset ] [ text "start over" ]
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
