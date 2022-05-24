module Jugs exposing (Jug(..), JugValue, Jugs, Msg(..), Step, Steps, applyStep, createJug, createJugs, emptyJug, fillJug, getCapacity, getJug, isSolved, main, pour, updateJug)

import Browser
import Html exposing (Html, button, div, h1, h2, li, ol, p, text)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)


type Jug
    = Gallon3
    | Gallon5


type alias JugValue =
    ( Jug, Int )


type alias Jugs =
    ( JugValue, JugValue )


type alias Step =
    Msg


type Msg
    = Fill Jug
    | Empty Jug
    | Pour Jug Jug
    | Solve


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


type alias Model =
    { jugs : Jugs
    , steps : Steps
    }


initialModel : Model
initialModel =
    { jugs = createJugs 0 0
    , steps = []
    }


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "measure 4 gallons exactly" ]
        , div [ id "jugs" ]
            [ viewJug Gallon3 "3 gallon jug" model.jugs
            , div [ id "pour-buttons" ]
                [ viewPourButton Gallon3 Gallon5 " >> "
                , viewPourButton Gallon5 Gallon3 " << "
                ]
            , viewJug Gallon5 "5 gallon jug" model.jugs
            ]
        , viewSteps model.steps
        ]


viewJug : Jug -> String -> Jugs -> Html Msg
viewJug jug jugLabel jugs =
    div [ class "jug" ]
        [ h2 [] [ text (jugLabel ++ ": " ++ String.fromInt (getJug jug jugs)) ]
        , button [ onClick (Fill jug) ] [ text "fill" ]
        , button [ onClick (Empty jug) ] [ text "empty" ]
        ]


viewPourButton : Jug -> Jug -> String -> Html Msg
viewPourButton source target description =
    div [] [ button [ onClick (Pour source target) ] [ text description ] ]


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

        _ ->
            ""


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

        -- default case for Msg variants that are not jug actions
        _ ->
            jugs


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Solve ->
        --     ( { model | jugs = applyStep msg model.jugs }, Cmd.none )
        _ ->
            ( { model | steps = model.steps ++ [ msg ], jugs = applyStep msg model.jugs }, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }
