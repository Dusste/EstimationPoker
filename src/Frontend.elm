module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Css
import Css.Global
import Html.Styled as Html exposing (Html, text)
import Html.Styled.Attributes as Attr
import Html.Styled.Events exposing (onClick, onInput)
import Lamdera exposing (sendToBackend)
import Process
import Tailwind.Breakpoints as Breakpoints
import Tailwind.Theme as Tw
import Tailwind.Utilities as Tw
import Task
import Time
import Types exposing (..)
import Url exposing (Url)
import Util


type alias Model =
    FrontendModel


validateInput : Maybe String -> Result InvalidTextFiled ValidTextField
validateInput maybeStr =
    case maybeStr of
        Nothing ->
            Err <| Just "Input is empty"

        Just str ->
            let
                trimmedStr =
                    String.trim str
            in
            if String.length trimmedStr > 3 then
                Ok trimmedStr

            else
                Err <| Just "Input is invalid"


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions =
            \m ->
                if m.shouldStartClock then
                    Time.every 1000 Tick

                else
                    Sub.none
        , view = view
        }


initialModel : Url -> Nav.Key -> Model
initialModel url key =
    { key = key
    , url = Url.toString url
    , status = EnterAdminNameStep
    , name = Nothing
    , roomName = Nothing
    , story = Nothing
    , error = Nothing
    , roomId = Nothing
    , stories = []
    , credentials = Admin
    , users = []
    , clientId = Nothing
    , clock = 0
    , shouldStartClock = False
    }


init : Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    case Util.parseUrl url of
        Home ->
            ( initialModel url key, Cmd.none )

        NotFound ->
            let
                mdl =
                    initialModel url key
            in
            ( { mdl | status = Step404 }
            , Cmd.none
            )

        InviteRoute roomId ->
            let
                mdl =
                    initialModel url key
            in
            ( { mdl | status = EnterNameStep, roomId = Just roomId, credentials = Employee }
            , sendToBackend <| ReqRoomRoute roomId False
            )

        RoomRoute roomId ->
            let
                mdl =
                    initialModel url key
            in
            ( { mdl | roomId = Just roomId }, sendToBackend <| ReqRoomRoute roomId True )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            case Util.parseUrl url of
                Home ->
                    ( initialModel url model.key, Cmd.none )

                InviteRoute roomId ->
                    ( model, sendToBackend <| ReqRoomRoute roomId False )

                RoomRoute roomId ->
                    ( { model | status = PokerStep }, sendToBackend <| ReqRoomRoute roomId True )

                NotFound ->
                    ( model, Cmd.none )

        SendName cred ->
            case validateInput model.name of
                Err errorMessage ->
                    ( { model | error = errorMessage }, Cmd.none )

                Ok validInput ->
                    case cred of
                        Admin ->
                            -- TODO fix cleintId not use maybe
                            ( { model | status = CreateRoomStep, error = Nothing, name = Nothing, users = { defaultUser | clientId = model.clientId |> Maybe.withDefault "123", name = validInput, isAdmin = True } :: model.users }
                            , sendToBackend <| SendAdminNameToBE validInput
                            )

                        Employee ->
                            case model.clientId of
                                Just clientId ->
                                    ( { model | status = PokerStep, error = Nothing, name = Nothing, users = { defaultUser | clientId = clientId, name = validInput } :: model.users }
                                    , Cmd.batch [ sendToBackend <| SendUserNameToBE validInput (model.roomId |> Maybe.withDefault 1), Nav.pushUrl model.key <| "/room/" ++ (model.roomId |> Maybe.withDefault 1 |> String.fromInt) ]
                                    )

                                Nothing ->
                                    ( model, Cmd.none )

        SendRoom ->
            case validateInput model.roomName of
                Err errorMessage ->
                    ( { model | error = errorMessage }, Cmd.none )

                Ok validInput ->
                    let
                        roomId =
                            model.roomId
                                |> Maybe.withDefault 1
                    in
                    ( { model | status = CreateStoryStep, error = Nothing }
                    , sendToBackend <| SendRoomNameToBE validInput roomId
                    )

        SendStory ->
            case validateInput model.story of
                Err errorMessage ->
                    ( { model | error = errorMessage }, Cmd.none )

                Ok validInput ->
                    ( { model | error = Nothing, story = Nothing, stories = validInput :: model.stories }, Cmd.none )

        SaveStory ->
            case validateInput model.story of
                Err errorMessage ->
                    ( { model | error = errorMessage }, Cmd.none )

                Ok validInput ->
                    let
                        roomId =
                            model.roomId |> Maybe.withDefault 1

                        -- TODO think about something smarter
                        updatedStories =
                            validInput :: model.stories
                    in
                    ( { model | error = Nothing, status = PokerStep, story = Nothing, stories = updatedStories }
                    , Cmd.batch [ sendToBackend <| SendStoryToBE updatedStories roomId, Nav.pushUrl model.key <| "/room/" ++ (model.roomId |> Maybe.withDefault 1 |> String.fromInt) ]
                    )

        StoreName str ->
            ( { model | name = Just str }, Cmd.none )

        StoreRoom str ->
            ( { model | roomName = Just str }, Cmd.none )

        StoreStory str ->
            ( { model | story = Just str }, Cmd.none )

        ChooseCard cardValue ->
            let
                justClientId =
                    model.clientId |> Maybe.withDefault "123"

                updatedUsers =
                    model.users
                        |> List.map
                            (\user ->
                                if user.clientId == justClientId then
                                    { user | card = cardValue }

                                else
                                    user
                            )
            in
            ( { model | users = updatedUsers }, Cmd.none )

        Tick _ ->
            ( { model | clock = model.clock + 1 }, Cmd.none )

        StartTime ->
            ( { model | shouldStartClock = True }, Cmd.none )

        ResetTime ->
            ( { model | shouldStartClock = False, clock = 0 }, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        SendRoomIdToFE roomId ->
            ( { model | roomId = Just roomId }, Cmd.none )

        ResRoomRoute { status, roomName, clientId, stories, users } ->
            ( { model | status = status, roomName = Just roomName, clientId = Just clientId, stories = stories, users = users }, Cmd.none )

        UpdateRoom { clientId, name } ->
            ( { model | users = { defaultUser | clientId = clientId, name = name } :: model.users }, Cmd.none )

        SupplyBEData { stories, users } ->
            ( { model | stories = stories, users = users }, Cmd.none )

        UpdateUsers users ->
            ( { model | users = users }, Cmd.none )


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


view : Model -> Browser.Document FrontendMsg
view model =
    { title = ""
    , body =
        [ Html.toUnstyled <|
            Html.div []
                [ Html.div [ Attr.css [ Tw.font_sans ] ]
                    [ case model.error of
                        Just error ->
                            Html.p
                                [ Attr.css
                                    [ Tw.bg_color Tw.red_600
                                    , Tw.text_color Tw.white
                                    , Tw.p_1
                                    ]
                                ]
                                [ text error ]

                        Nothing ->
                            text ""
                    , Html.div []
                        [ text <| fromIntToCounter model.clock ]
                    , case model.status of
                        EnterAdminNameStep ->
                            Html.div []
                                [ Html.div [] [ Html.h3 [] [ text "Add your name" ] ]
                                , Html.div []
                                    [ Html.p [] [ text "(You are about to become an admin)" ]
                                    , Html.input
                                        [ Attr.type_ "text"
                                        , onInput StoreName
                                        , model.name
                                            |> Maybe.withDefault ""
                                            |> Attr.value
                                        ]
                                        []
                                    ]
                                , Html.div []
                                    [ Html.button [ onClick <| SendName Admin ] [ text "Save" ]
                                    ]
                                ]

                        EnterNameStep ->
                            Html.div []
                                [ Html.div [] [ Html.h3 [] [ model.roomName |> Maybe.withDefault "{default room name}" |> text ] ]
                                , Html.div []
                                    [ Html.p [] [ text "Add your name" ]
                                    , Html.p [] [ text "(After that we will redirect you to team's room)" ]
                                    , Html.input
                                        [ Attr.type_ "text"
                                        , onInput StoreName
                                        , model.name
                                            |> Maybe.withDefault ""
                                            |> Attr.value
                                        ]
                                        []
                                    ]
                                , Html.div []
                                    [ Html.button [ onClick <| SendName Employee ] [ text "Save" ]
                                    ]
                                ]

                        CreateRoomStep ->
                            Html.div []
                                [ Html.div [] [ Html.h3 [] [ text "Create new room" ] ]
                                , Html.div []
                                    [ Html.input
                                        [ Attr.type_ "text"
                                        , onInput StoreRoom
                                        , model.roomName
                                            |> Maybe.withDefault ""
                                            |> Attr.value
                                        ]
                                        []
                                    ]
                                , Html.div []
                                    [ Html.button [ onClick SendRoom ] [ text "Create" ]
                                    ]
                                ]

                        CreateStoryStep ->
                            Html.div []
                                [ Html.div [] [ Html.h3 [] [ text "Create new story" ] ]
                                , Html.div []
                                    [ Html.input
                                        [ Attr.type_ "text"
                                        , onInput StoreStory
                                        , model.story
                                            |> Maybe.withDefault ""
                                            |> Attr.value
                                        ]
                                        []
                                    ]
                                , Html.div []
                                    [ Html.button [ onClick SendStory ] [ text "Save and Add new" ]
                                    , Html.button [ onClick SaveStory ] [ text "Save and Close" ]
                                    ]
                                ]

                        PokerStep ->
                            Html.div []
                                [ Html.div [] [ Html.h3 [] [ model.roomName |> Maybe.withDefault "{default room name}" |> text ] ]
                                , Html.div []
                                    [ text "I am main content"
                                    , Html.div []
                                        [ viewCards ]
                                    ]
                                , Html.div []
                                    [ text "I am sidebar. Copy link and send to collegues to invite"
                                    , Html.input [ Attr.readonly True, Attr.value <| model.url ++ "invite/" ++ (model.roomId |> Maybe.withDefault 1 |> String.fromInt) ] []
                                    , Html.div []
                                        [ text "Users:"
                                        , Html.ul []
                                            (model.users
                                                |> List.map
                                                    (\{ isAdmin, name } ->
                                                        if isAdmin then
                                                            Html.li [ Attr.css [ Tw.text_color Tw.blue_400 ] ] [ text name ]

                                                        else
                                                            Html.li [] [ text name ]
                                                    )
                                            )
                                        ]
                                    ]
                                , Html.div []
                                    [ case model.credentials of
                                        Admin ->
                                            Html.div []
                                                [ if model.shouldStartClock then
                                                    Html.div []
                                                        [ Html.button [ onClick ResetTime ] [ text "Reset timer" ]
                                                        , Html.button [ onClick ResetTime ] [ text "Flip cards" ]
                                                        , Html.button [ onClick ResetTime ] [ text "Clear votes" ]
                                                        , Html.button [ onClick ResetTime ] [ text "Skip story" ]
                                                        ]

                                                  else
                                                    Html.button [ onClick StartTime ] [ text "Start timer" ]
                                                ]

                                        Employee ->
                                            Html.div [] []
                                    ]
                                , Html.div []
                                    [ text "I am bottom tabs, I present stories"
                                    ]
                                , Html.ul []
                                    (model.stories
                                        |> List.map
                                            (\story ->
                                                Html.li [] [ text story ]
                                            )
                                    )
                                ]

                        Step404 ->
                            Html.div []
                                [ Html.div []
                                    [ Html.h2 [] [ text "4 ō 4 bud..." ]
                                    , Html.p [] [ text "Nothing to see here -_-" ]
                                    , Html.a [ Attr.href "/" ] [ text "Go Home" ]
                                    ]
                                ]
                    ]
                ]
        ]
    }


cards : List { name : String, value : Float }
cards =
    [ { name = "0", value = 0 }
    , { name = "1/2", value = 1.5 }
    , { name = "1", value = 1 }
    , { name = "2", value = 2 }
    , { name = "3", value = 3 }
    , { name = "5", value = 5 }
    , { name = "13", value = 13 }
    , { name = "20", value = 20 }
    , { name = "40", value = 40 }
    , { name = "100", value = 100 }
    , { name = "?", value = 0 }
    , { name = "Coffee Breaker", value = 0 }
    ]


viewCards : Html FrontendMsg
viewCards =
    Html.ul []
        (cards
            |> List.map
                (\card ->
                    Html.li [ onClick <| ChooseCard card.value ]
                        [ Html.span [] [ text card.name ]
                        ]
                )
        )
