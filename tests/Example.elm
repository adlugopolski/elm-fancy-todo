module Example exposing (..)

import Html exposing (text)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import View
import Types

suite : Test
suite =
  describe "View.completedButton" 
    [ 
      test "with no completed entries" <|
        \_ ->
          let
            entries = 
              [
                { completed = False }
              ]
          in
            Expect.equal text(View.completedButton(entries)), text("x")
    ]
