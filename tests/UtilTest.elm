module UtilTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Types exposing (Sequence(..))
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


customSequenceTest : Test
customSequenceTest =
    describe "From string to sequence"
        [ test "output should be free from letters input" <|
            \_ ->
                Util.fromStringToSequence "mmmmmmmmmmm"
                    |> Expect.equal (InTransition "")
        , test "output should be without space from number input followed by space (this can happen only when pasting string)" <|
            \_ ->
                Util.fromStringToSequence "1 2 3 4 5 6 7 8 9 10 11 12"
                    |> Expect.equal (Accept "1 2 3 4 5 6 7 8 9 10 11 12")
        , test "output should be free from letters and special characters when combining them with numbers" <|
            \_ ->
                Util.fromStringToSequence "   1f$#@d-3 %@#@%#3fd21 f%$#d32 kmk2m1231 f$%#d1 df3%$#%d12 3f-d12 fd@!33 fd@!_213 2d)(_f1 d*&f321 d(f3  hu7  "
                    |> Expect.equal (Accept "13 321 32 212 1 312 312 33 213 21 321 3")
        , test "output should have each sequence of numbers trimmed to 3 digits when it has 12 of them, if exceeds that threshold" <|
            \_ ->
                Util.fromStringToSequence "33221321321312 1233232131231 3212313212133432 21 321 321 3 78 76 767767676 767 23"
                    |> Expect.equal (Accept "332 123 321 21 321 321 3 78 76 767 767 23")
        , test "output should ignore set of only letters in unit and keep on extracting numbers from string" <|
            \_ ->
                Util.fromStringToSequence " sdaddsa dsadsakmdasklmdklsa lkmdsakmda ddasd1233232131231 s3212313212133432 988327423 s321 s321 h3 l78 p76 m767 jn4dsjsn6##@$4%^%^99999 djksnkjndas2s3s s23njknj21s3s k231321knks k213k231s "
                    |> Expect.equal (Accept "123 321 988 321 321 3 78 76 767 464 23 232")
        , test "output should be accepted since we have 12 sets of numbers" <|
            \_ ->
                Util.fromStringToSequence "1 2 3 4 5 6 7 8 9 10 11 12"
                    |> Expect.equal (Accept "1 2 3 4 5 6 7 8 9 10 11 12")
        , test "output should be InTransition since we have 11 sets of numbers" <|
            \_ ->
                Util.fromStringToSequence "1 2 3 4 5 6 7 8 9 10 11"
                    |> Expect.equal (InTransition "1 2 3 4 5 6 7 8 9 10 11")
        , test "output should be trimmed for string that exceeds 12 sets of numbers" <|
            \_ ->
                Util.fromStringToSequence "    1f$#@d-3 %@#@%#3fd21 f%$#d32 kmk2m1231 f$%#d1 df3%$#%d12 3f-d12 fd@!33 fd@!_213 2d)(_f1 d*&f321 d(f3  13thNumber      "
                    |> Expect.equal (Accept "13 321 32 212 1 312 312 33 213 21 321 3")
        ]
