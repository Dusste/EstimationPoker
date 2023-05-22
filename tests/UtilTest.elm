module UtilTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Util


counterTest : Test
counterTest =
    describe "From int to counter"
        [ test "output should be 2:30 if seconds 150" <|
            \_ ->
                Util.fromIntToCounter 150
                    |> Expect.equal "02:30"
        , test "output should be 38:20 if seconds 2300" <|
            \_ ->
                Util.fromIntToCounter 2300
                    |> Expect.equal "38:20"
        , test "output should be 00:00 if seconds 0" <|
            \_ ->
                Util.fromIntToCounter 0
                    |> Expect.equal "00:00"
        , test "output should be 00:35 if seconds 35" <|
            \_ ->
                Util.fromIntToCounter 35
                    |> Expect.equal "00:35"
        ]
