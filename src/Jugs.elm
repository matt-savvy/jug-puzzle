module Jugs exposing (Jug(..), Jugs, emptyJug, fillJug, getCapacity, getJug, main, pour, updateJug)

import Browser
import Html exposing (Html, button, div, h1, h2, p, text)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)


type Jug
    = Gallon3
    | Gallon5


type alias Jugs =
    ( ( Jug, Int ), ( Jug, Int ) )


initJugs : Jugs
initJugs =
    ( ( Gallon3, 3 )
    , ( Gallon5, 5 )
    )


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


type alias Model =
    { jugs : Jugs }


initialModel : Model
initialModel =
    { jugs =
        ( ( Gallon3, 0 )
        , ( Gallon5, 0 )
        )
    }


type Msg
    = Fill Jug
    | Empty Jug
    | Pour Jug Jug


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Fill jug ->
            ( { model | jugs = updateJug jug (getCapacity jug) model.jugs }, Cmd.none )

        Empty jug ->
            ( { model | jugs = emptyJug jug model.jugs }, Cmd.none )

        Pour source target ->
            ( { model | jugs = pour source target model.jugs }, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }
