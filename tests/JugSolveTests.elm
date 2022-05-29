module JugSolveTests exposing (jugSolverTest)

import Expect
import Jugs exposing (Jug(..), Step(..), Steps(..), createJugs, jugSolver)
import Test exposing (Test, test)


solution : Steps
solution =
    Steps
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
