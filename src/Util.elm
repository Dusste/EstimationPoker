module Util exposing (..)

import Types exposing (..)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser)


parseUrl : Url -> Route
parseUrl url =
    case Parser.parse matchRoute url of
        Just route ->
            route

        Nothing ->
            NotFound


roomIdParser : Parser (RoomParam -> a) a
roomIdParser =
    Parser.custom "ROOMPARAM" <|
        \roomParam ->
            String.toInt roomParam


matchRoute : Parser (Route -> a) a
matchRoute =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map InviteRoute (Parser.s "invite" </> roomIdParser)
        , Parser.map RoomRoute (Parser.s "room" </> roomIdParser)
        ]
