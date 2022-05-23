module JugSolveTests exposing (..)

import Expect exposing (Expectation)
import JugSolve exposing (..)
import Jugs exposing (..)
import Test exposing (..)


solution : Steps
solution =
    [ Fill Gallon5
    , Pour Gallon5 Gallon3
    , Empty Gallon3
    , Pour Gallon5 Gallon3
    , Fill Gallon5
    , Pour Gallon5 Gallon3
    ]


jugSolverTest : Test
jugSolverTest =
    test "find shortest solution" <| \_ -> Expect.equal (jugSolver (createJugs 0 0)) solution
