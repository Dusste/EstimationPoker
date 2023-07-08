module Util exposing (..)

import Browser.Events exposing (onKeyDown)
import Dict exposing (Dict)
import Html.Styled as Html exposing (Attribute)
import Html.Styled.Events exposing (keyCode, on)
import Json.Decode as Json
import Parser exposing ((|.), (|=), Step(..))
import Tailwind.Theme as Tw
import Tailwind.Utilities as Tw
import Types exposing (..)
import Url exposing (Url)
import Url.Parser as UrlParser exposing ((</>))


parseUrl : Url -> Route
parseUrl url =
    case UrlParser.parse matchRoute url of
        Just route ->
            route

        Nothing ->
            NotFound


roomIdParser : UrlParser.Parser (RoomParam -> a) a
roomIdParser =
    UrlParser.custom "ROOMPARAM" <|
        \roomParam ->
            String.toInt roomParam


matchRoute : UrlParser.Parser (Route -> a) a
matchRoute =
    UrlParser.oneOf
        [ UrlParser.map Home UrlParser.top
        , UrlParser.map InviteRoute (UrlParser.s "invite" </> roomIdParser)
        , UrlParser.map RoomRoute (UrlParser.s "room" </> roomIdParser)
        ]


fromIntToCounter : Int -> String
fromIntToCounter interval =
    let
        diffMinutes =
            interval
                // 60

        diffSeconds =
            modBy 60 interval

        minutes =
            if diffMinutes >= 10 then
                diffMinutes |> String.fromInt

            else
                "0" ++ (diffMinutes |> String.fromInt)

        seconds =
            if diffSeconds >= 10 then
                diffSeconds |> String.fromInt

            else
                "0" ++ (diffSeconds |> String.fromInt)
    in
    minutes ++ ":" ++ seconds


getBaseUrl : Url -> String
getBaseUrl u =
    case u.port_ of
        Nothing ->
            "https://" ++ u.host ++ "/"

        Just p ->
            "http://"
                ++ u.host
                ++ ":"
                ++ (p |> String.fromInt)
                ++ "/"


toChartData : Float -> Float -> List User -> List ChartsData
toChartData increment teamSize sortedLst =
    case sortedLst of
        [] ->
            []

        x :: xs ->
            let
                listOfLeftValues =
                    xs |> List.map (\u -> u.card)
            in
            if List.isEmpty xs then
                { numOfVoters = increment, percentage = increment / teamSize * 100, uniqueVoteValue = x.card } :: toChartData 1 teamSize []

            else if List.member x.card listOfLeftValues then
                [] ++ toChartData (increment + 1) teamSize xs

            else
                { uniqueVoteValue = x.card, percentage = increment / teamSize * 100, numOfVoters = increment } :: toChartData 1 teamSize xs


getColor : Int -> Tw.Color
getColor target =
    colorConfig
        |> Dict.get target
        |> Maybe.withDefault Tw.teal_400


getHexColor : Int -> String
getHexColor target =
    colorConfig
        |> Dict.get target
        |> fromTWcolorToHex
        |> Maybe.withDefault "#2dd4bf"


fromTWcolorToHex : Maybe Tw.Color -> Maybe String
fromTWcolorToHex twCol =
    twCol
        |> Maybe.andThen
            (\color ->
                if color == Tw.pink_400 then
                    Just "#f472b6"

                else if color == Tw.sky_400 then
                    Just "#38bdf8"

                else if color == Tw.lime_400 then
                    Just "#a3e635"

                else if color == Tw.purple_900 then
                    Just "#581c87"

                else if color == Tw.sky_700 then
                    Just "#0369a1"

                else if color == Tw.lime_500 then
                    Just "#84cc16"

                else if color == Tw.pink_700 then
                    Just "#be185d"

                else if color == Tw.teal_700 then
                    Just "#0f766e"

                else if color == Tw.lime_900 then
                    Just "#365314"

                else if color == Tw.teal_800 then
                    Just "#115e59"

                else if color == Tw.pink_200 then
                    Just "#fbcfe8"

                else if color == Tw.lime_300 then
                    Just "#bef264"

                else if color == Tw.teal_500 then
                    Just "#14b8a6"

                else if color == Tw.sky_600 then
                    Just "#0284c7"

                else if color == Tw.lime_700 then
                    Just "#4d7c0f"

                else if color == Tw.teal_900 then
                    Just "#134e4a"

                else if color == Tw.lime_600 then
                    Just "#65a30d"

                else if color == Tw.pink_300 then
                    Just "#f9a8d4"

                else if color == Tw.sky_500 then
                    Just "#0ea5e9"

                else if color == Tw.teal_200 then
                    Just "#99f6e4"

                else if color == Tw.pink_500 then
                    Just "#ec4899"

                else if color == Tw.teal_300 then
                    Just "#5eead4"

                else if color == Tw.lime_200 then
                    Just "#d9f99d"

                else if color == Tw.sky_900 then
                    Just "#0c4a6e"

                else if color == Tw.pink_600 then
                    Just "#db2777"

                else if color == Tw.teal_600 then
                    Just "#0d9488"

                else if color == Tw.sky_800 then
                    Just "#075985"

                else if color == Tw.pink_900 then
                    Just "#831843"

                else if color == Tw.sky_200 then
                    Just "#bae6fd"

                else if color == Tw.lime_800 then
                    Just "#3f6212"

                else if color == Tw.pink_800 then
                    Just "#9d174d"

                else if color == Tw.sky_300 then
                    Just "#7dd3fc"

                else
                    Nothing
            )


colorConfig : Dict Int Tw.Color
colorConfig =
    Dict.fromList
        [ ( 0, Tw.pink_400 )
        , ( 1, Tw.sky_400 )
        , ( 2, Tw.lime_400 )
        , ( 3, Tw.purple_900 )
        , ( 4, Tw.sky_700 )
        , ( 5, Tw.lime_500 )
        , ( 9, Tw.pink_700 )
        , ( 8, Tw.teal_700 )
        , ( 11, Tw.lime_900 )
        , ( 10, Tw.teal_800 )
        , ( 15, Tw.pink_200 )
        , ( 7, Tw.lime_300 )
        , ( 6, Tw.teal_500 )
        , ( 12, Tw.sky_600 )
        , ( 14, Tw.lime_700 )
        , ( 13, Tw.teal_900 )
        , ( 16, Tw.lime_600 )
        , ( 17, Tw.pink_300 )
        , ( 18, Tw.sky_500 )
        , ( 19, Tw.teal_200 )
        , ( 20, Tw.pink_500 )
        , ( 21, Tw.teal_300 )
        , ( 22, Tw.lime_200 )
        , ( 23, Tw.sky_900 )
        , ( 24, Tw.pink_600 )
        , ( 25, Tw.teal_600 )
        , ( 26, Tw.sky_800 )
        , ( 27, Tw.pink_900 )
        , ( 28, Tw.sky_200 )
        , ( 29, Tw.lime_800 )
        , ( 30, Tw.pink_800 )
        , ( 31, Tw.sky_300 )

        -- It's highly unlikely to get more then 31 participants
        ]


getCommonSequenceConfig : CommonSequence -> List SequenceConfig
getCommonSequenceConfig chooseSequence =
    [ { msg = SelectSequence, choosenSequence = chooseSequence, sequenceValue = defaultSequenceValues, msgAttribute = Default, borderSetup = [ Tw.border_l_0, Tw.border_t_0 ], textValue = "Default" }
    , { msg = SelectSequence, choosenSequence = chooseSequence, sequenceValue = option2SequenceValues, msgAttribute = Option2, borderSetup = [ Tw.border_r_0, Tw.border_t_0 ], textValue = "Option 2" }
    , { msg = SelectSequence, choosenSequence = chooseSequence, sequenceValue = option3SequenceValues, msgAttribute = Option3, borderSetup = [ Tw.border_b_0, Tw.border_l_0 ], textValue = "Option 3" }
    , { msg = SelectSequence, choosenSequence = chooseSequence, sequenceValue = option4SequenceValues, msgAttribute = Option4, borderSetup = [ Tw.border_b_0, Tw.border_r_0 ], textValue = "Option 4" }
    ]


defaultCards : List { name : String, value : Float }
defaultCards =
    [ { name = "0", value = 0 }
    , { name = "1/2", value = 0.5 }
    , { name = "1", value = 1 }
    , { name = "2", value = 2 }
    , { name = "3", value = 3 }
    , { name = "5", value = 5 }
    , { name = "13", value = 13 }
    , { name = "20", value = 20 }
    , { name = "40", value = 40 }
    , { name = "100", value = 100 }
    , { name = "?", value = -1 }
    , { name = 0x2615 |> Char.fromCode |> String.fromChar, value = 0 }
    ]


fromStringToCards : SequenceString -> List { name : String, value : Float }
fromStringToCards seqStr =
    seqStr
        |> String.split ","
        |> List.map
            (\str ->
                let
                    trimmedStringInt =
                        str |> String.trim
                in
                case trimmedStringInt |> String.toInt of
                    Just int ->
                        { name = trimmedStringInt, value = toFloat int }

                    Nothing ->
                        let
                            val =
                                case str of
                                    "?" ->
                                        toFloat -1

                                    "1/2" ->
                                        0.5

                                    _ ->
                                        0
                        in
                        { name = trimmedStringInt, value = val }
            )


defaultSequenceValues : SequenceString
defaultSequenceValues =
    "0, 1/2, 1, 2, 3, 5, 13, 20, 40, 100, ?," ++ (0x2615 |> Char.fromCode |> String.fromChar)


option2SequenceValues : SequenceString
option2SequenceValues =
    "1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12"


option3SequenceValues : SequenceString
option3SequenceValues =
    "10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120"


option4SequenceValues : SequenceString
option4SequenceValues =
    "1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12"


fromSequenceToCards : CommonSequence -> List { name : String, value : Float }
fromSequenceToCards seq =
    case seq of
        Default ->
            defaultCards

        Option2 ->
            fromStringToCards option2SequenceValues

        Option3 ->
            fromStringToCards option3SequenceValues

        Option4 ->
            fromStringToCards option4SequenceValues

        CustomSequence str ->
            fromStringToCards str


roundFloat : Float -> Int -> String
roundFloat flt roundToNumberOfPlaces =
    let
        splitedString : List String
        splitedString =
            flt |> String.fromFloat |> String.split "."
    in
    case splitedString of
        [] ->
            flt |> String.fromFloat

        [ _ ] ->
            flt |> String.fromFloat

        firstPart :: secondPart :: _ ->
            let
                slicedSecondPart =
                    secondPart |> String.slice 0 roundToNumberOfPlaces
            in
            [ firstPart, slicedSecondPart ] |> String.join "."


pluralize : Float -> String -> String
pluralize num initialString =
    if num == 1 then
        initialString

    else
        initialString ++ "s"


onEnter : FrontendMsg -> Attribute FrontendMsg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg

            else
                Json.fail "not ENTER"
    in
    on "keydown" (Json.andThen isEnter keyCode)


validateInput : String -> Result String ValidTextField
validateInput string =
    let
        fromStringToResult : String -> Result String (String -> ValidTextField) -> Result String ValidTextField
        fromStringToResult input =
            Result.andThen
                (\constructor ->
                    if String.trim input |> String.isEmpty then
                        Err "Input is empty"

                    else
                        Ok (constructor (String.trim input))
                )
    in
    Ok ValidTextField
        |> fromStringToResult string


fromValidTextFieldToString : ValidTextField -> String
fromValidTextFieldToString (ValidTextField string) =
    string


itemHelp : List String -> Parser.Parser (Step (List String) (List String))
itemHelp nums =
    let
        checkNum numsSoFar num =
            let
                isDigit char =
                    Char.isDigit char

                filterDuplicates lst =
                    case lst of
                        [] ->
                            []

                        x :: xs ->
                            if List.member x xs then
                                filterDuplicates xs

                            else
                                x :: filterDuplicates xs

                sortStringedIntsAndSpecialCases : List String -> List String
                sortStringedIntsAndSpecialCases lst =
                    let
                        isSpecialCase str =
                            String.contains str "1/2" || str == "?"

                        ( specialCases, intsButStrings ) =
                            List.partition isSpecialCase lst

                        sortedIntsButStrings =
                            intsButStrings |> List.filterMap String.toInt |> List.sort |> List.map String.fromInt
                    in
                    specialCases ++ sortedIntsButStrings
            in
            if String.length num > 0 then
                Loop (num :: numsSoFar)

            else
                {-
                   Clean list at the end since we can get string-only item in list, which will result in bail out early, stoping us from proceeding
                   eg: " sdaddsa dsadsakmdasklmdklsa lkmdsakmda 33221321321312 1233232131231 3212313212133432 21 321 321 3 78 76 767767676 767 23"
                -}
                Done
                    (List.reverse numsSoFar
                        |> List.map
                            (\str ->
                                if String.contains "?" str then
                                    " ? "

                                else if String.contains "1/2" str then
                                    " 1/2 "

                                else
                                    str
                            )
                        -- Allow story to be estimated with "?" or "1/2"
                        |> List.map (String.filter (\c -> isDigit c || c == '/' || c == '?'))
                        |> List.map
                            (\str ->
                                if String.startsWith "0" str && String.length str > 1 then
                                    String.toInt str |> Maybe.withDefault 0 |> String.fromInt

                                else
                                    str
                            )
                        |> List.map (String.slice 0 3)
                        |> List.filter (not << String.isEmpty)
                        |> filterDuplicates
                        |> sortStringedIntsAndSpecialCases
                    )
    in
    Parser.succeed (checkNum nums)
        |. spacesParser
        |= (Parser.getChompedString <| Parser.chompWhile (\c -> c /= ' '))
        |. spacesParser


itemUnitList : Parser.Parser (List String)
itemUnitList =
    Parser.loop [] itemHelp


spacesParser : Parser.Parser ()
spacesParser =
    Parser.chompWhile (\c -> c == ' ')


sequence : Parser.Parser Sequence
sequence =
    let
        sizeLimit =
            12

        hasReachedLimit lst =
            List.length lst >= sizeLimit

        checkSequence : List String -> Parser.Parser Sequence
        checkSequence lst =
            if hasReachedLimit lst then
                Parser.succeed <| Accept (lst |> List.take sizeLimit |> String.join " ")

            else
                Parser.succeed <| InTransition (lst |> String.join " ")
    in
    itemUnitList
        |> Parser.andThen checkSequence



{-
   It takes input and produce unit of numbers divided by space
    - Trim all special characters except: "1/2" and "?", those can live only as separate unit
    - Trim zeros from beggining of unit
    - Trim excess of numbers per unit, 3 numbers per unit permited
    - It consider input as "Accept" when it has 12 units, otherwise it's in state of "InTransition"
    - If it has unit containing only non-digits, it will skip it
    - It's sorts and duplicates are filtered, on beggining of list it should be "1/2" and "?", if present, followed by ints

-}


fromStringToSequence : String -> Sequence
fromStringToSequence input =
    case Parser.run sequence input of
        Ok validSequence ->
            validSequence

        Err _ ->
            Reject
