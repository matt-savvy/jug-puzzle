module Jugs exposing (Jug(..), Jugs, getJug, main, updateJug)

import Browser
import Html exposing (Html, button, div, h1, h2, text)
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


type alias Model =
    { gallon5 : Int
    , gallon3 : Int
    }


initialModel : Model
initialModel =
    { gallon5 = 2, gallon3 = 0 }


type Msg
    = FillGallon5
    | EmptyGallon5
    | FillGallon3
    | EmptyGallon3
    | Pour3To5
    | Pour5To3


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "nice jugs" ]
        , div []
            [ h2 [] [ text ("3 gallon jug " ++ String.fromInt model.gallon3) ]
            , h2 [] [ text ("5 gallon jug " ++ String.fromInt model.gallon5) ]
            , button [ onClick FillGallon3 ] [ text "fill 3 gallon jug" ]
            , button [ onClick FillGallon5 ] [ text "fill 5 gallon jug" ]
            , button [ onClick EmptyGallon3 ] [ text "empty 3 gallon jug" ]
            , button [ onClick EmptyGallon5 ] [ text "empty 5 gallon jug" ]
            , button [ onClick Pour3To5 ] [ text "pour 3 gallon into 5 gallon" ]
            , button [ onClick Pour5To3 ] [ text "pour 5 gallon into 3 gallon" ]
            ]
        ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        FillGallon5 ->
            { model | gallon5 = 5 }

        FillGallon3 ->
            { model | gallon3 = 3 }

        EmptyGallon5 ->
            { model | gallon5 = 0 }

        EmptyGallon3 ->
            { model | gallon3 = 0 }

        Pour3To5 ->
            let
                spaceLeft =
                    5 - model.gallon5
            in
            if model.gallon3 >= spaceLeft then
                { model | gallon5 = 5, gallon3 = model.gallon3 - spaceLeft }

            else
                { model | gallon5 = model.gallon5 + model.gallon3, gallon3 = model.gallon3 - model.gallon3 }

        Pour5To3 ->
            let
                spaceLeft =
                    3 - model.gallon3

                amountToPour =
                    max spaceLeft model.gallon5
            in
            if model.gallon5 >= spaceLeft then
                { model | gallon5 = model.gallon5 - spaceLeft, gallon3 = 3 }

            else
                { model | gallon5 = model.gallon5 - model.gallon5, gallon3 = model.gallon3 + model.gallon5 }


main =
    Browser.sandbox
        { init = initialModel
        , update = update
        , view = view
        }
