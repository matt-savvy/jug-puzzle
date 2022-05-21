module Jugs exposing (Jug(..), Jugs, getJug, getMax, main, pour, updateJug)

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


getMax : Jug -> Int
getMax jug =
    case jug of
        Gallon3 ->
            3

        Gallon5 ->
            5


pour : Jug -> Jug -> Jugs -> Jugs
pour source target jugs =
    let
        spaceLeft =
            getMax target - getJug target jugs

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
    = FillGallon5
    | EmptyGallon5
    | FillGallon3
    | EmptyGallon3
    | Pour Jug Jug


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "nice jugs" ]
        , div []
            [ h2 [] [ text ("3 gallon jug " ++ String.fromInt (getJug Gallon3 model.jugs)) ]
            , h2 [] [ text ("5 gallon jug " ++ String.fromInt (getJug Gallon5 model.jugs)) ]
            , button [ onClick FillGallon3 ] [ text "fill 3 gallon jug" ]
            , button [ onClick FillGallon5 ] [ text "fill 5 gallon jug" ]
            , button [ onClick EmptyGallon3 ] [ text "empty 3 gallon jug" ]
            , button [ onClick EmptyGallon5 ] [ text "empty 5 gallon jug" ]
            , button [ onClick (Pour Gallon3 Gallon5) ] [ text "pour 3 gallon into 5 gallon" ]
            , button [ onClick (Pour Gallon5 Gallon3) ] [ text "pour 5 gallon into 3 gallon" ]
            ]
        ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        FillGallon5 ->
            { model | jugs = updateJug Gallon5 5 model.jugs }

        FillGallon3 ->
            { model | jugs = updateJug Gallon3 3 model.jugs }

        EmptyGallon5 ->
            { model | jugs = updateJug Gallon5 0 model.jugs }

        EmptyGallon3 ->
            { model | jugs = updateJug Gallon3 0 model.jugs }

        Pour source target ->
            { model | jugs = pour source target model.jugs }


main =
    Browser.sandbox
        { init = initialModel
        , update = update
        , view = view
        }
