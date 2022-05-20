module JugTests exposing (..)

import Jugs exposing (..)
import Expect exposing (Expectation)
import Test exposing (..)


pour5to3Test : Test
pour5to3Test =
    test "check" <|
        \_ -> Expect.equal 5 5
