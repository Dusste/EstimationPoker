module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Css
import Css.Global
import Html.Styled as Html exposing (Html, text)
import Html.Styled.Attributes as Attr
import Html.Styled.Events exposing (onClick, onInput)
import Lamdera exposing (sendToBackend)
import Svg.Styled exposing (path, svg)
import Svg.Styled.Attributes as SvgAttr
import Tailwind.Breakpoints as Breakpoints
import Tailwind.Theme as Tw
import Tailwind.Utilities as Tw
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
    , url = Util.getBaseUrl url -- TODO make it base url only: localhost:8000/
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
    , shouldFlipCards = False
    , shouldShowCharts = False
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
                    ( { model | error = Nothing, story = Nothing, stories = model.stories ++ [ validInput ] }, Cmd.none )

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
                            model.stories ++ [ validInput ]
                    in
                    ( { model | error = Nothing, status = PokerStep, story = Nothing, stories = updatedStories }
                    , Cmd.batch [ sendToBackend <| SendStoryToBE updatedStories roomId, Nav.pushUrl model.key <| "/room/" ++ (roomId |> String.fromInt) ]
                    )

        StoreName str ->
            ( { model | name = Just str }, Cmd.none )

        StoreRoom str ->
            ( { model | roomName = Just str }, Cmd.none )

        StoreStory str ->
            ( { model | story = Just str }, Cmd.none )

        ChooseCard cardValue ->
            case model.clientId of
                Just justClientId ->
                    let
                        updatedUsers =
                            model.users
                                |> List.map
                                    (\user ->
                                        if user.clientId == justClientId then
                                            { user | card = Just cardValue, hasVoted = True }

                                        else
                                            user
                                    )
                    in
                    ( { model | users = updatedUsers }, sendToBackend <| SendCard cardValue (model.roomId |> Maybe.withDefault 1) )

                Nothing ->
                    ( model, Cmd.none )

        Tick _ ->
            ( { model | clock = model.clock + 1 }, Cmd.none )

        StartTime ->
            ( { model | shouldStartClock = True }, sendToBackend <| StartTimerAndVote (model.roomId |> Maybe.withDefault 1) )

        ResetTime ->
            ( { model | shouldStartClock = False, clock = 0 }, sendToBackend <| ResetTimerAndVote (model.roomId |> Maybe.withDefault 1) )

        FlipCards ->
            ( { model | shouldFlipCards = not <| model.shouldFlipCards }, sendToBackend <| InitiateFlipCards (model.roomId |> Maybe.withDefault 1) )

        ClearVotes ->
            ( { model | shouldFlipCards = False }, sendToBackend <| ClearAllUserVotes (model.roomId |> Maybe.withDefault 1) )

        FinishVoting ->
            ( { model | shouldStartClock = False }, sendToBackend <| SignalShowCharts (model.roomId |> Maybe.withDefault 1) )

        NextStory ->
            let
                updatedStories =
                    model.stories |> List.drop 1
            in
            ( { model | stories = updatedStories }, Cmd.batch [ sendToBackend <| ClearAllUserVotes (model.roomId |> Maybe.withDefault 1), sendToBackend <| SignalUpdateStories updatedStories (model.roomId |> Maybe.withDefault 1) ] )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        SendRoomIdToFE roomId ->
            ( { model | roomId = Just roomId }, Cmd.none )

        ResRoomRoute { status, roomName, clientId, stories, users } ->
            let
                isAdmin =
                    users
                        |> List.filter (\user -> user.clientId == clientId)
                        |> List.head
                        |> Maybe.withDefault defaultUser
                        |> .isAdmin
            in
            ( { model
                | status = status
                , roomName = Just roomName
                , clientId = Just clientId
                , stories = stories
                , users = users
                , credentials =
                    if isAdmin then
                        Admin

                    else
                        Employee
              }
            , Cmd.none
            )

        UpdateRoom { clientId, name } ->
            ( { model | users = { defaultUser | clientId = clientId, name = name } :: model.users }, Cmd.none )

        SupplyBEData { stories, users } ->
            ( { model | stories = stories, users = users }, Cmd.none )

        UpdateUsers users ->
            ( { model | users = users }, Cmd.none )

        UsersStartTimer ->
            ( { model | shouldStartClock = True }, Cmd.none )

        UsersResetTimer ->
            ( { model | shouldStartClock = False, clock = 0 }, Cmd.none )

        UpdateCards users ->
            ( { model | users = users }, Cmd.none )

        UsersFlipCards ->
            ( { model | shouldFlipCards = not <| model.shouldFlipCards }, Cmd.none )

        UsersCardReset users ->
            ( { model | users = users, shouldFlipCards = False }, Cmd.none )

        ExposeCharts ->
            ( { model | shouldShowCharts = not <| model.shouldShowCharts, shouldStartClock = False, clock = 0 }, Cmd.none )

        UpdateStories updatedStories resetUsers ->
            ( { model | stories = updatedStories, shouldShowCharts = not <| model.shouldShowCharts, users = resetUsers }, Cmd.none )


view : Model -> Browser.Document FrontendMsg
view model =
    { title = "EstPoker"
    , body =
        [ Html.toUnstyled <|
            Html.div []
                [ Html.div [ Attr.css [ Tw.font_sans ] ]
                    [ case model.error of
                        Just error ->
                            Html.div
                                [ Attr.css
                                    [ Tw.p_4
                                    , Tw.flex
                                    , Tw.mb_4
                                    , Tw.text_sm
                                    , Tw.text_color Tw.red_800
                                    , Tw.rounded_lg
                                    , Tw.bg_color Tw.red_50
                                    ]
                                , Attr.attribute "role" "alert"
                                ]
                                [ svg
                                    [ Attr.css
                                        [ Tw.flex_shrink_0
                                        , Tw.inline
                                        , Tw.w_5
                                        , Tw.h_5
                                        , Tw.mr_3
                                        ]
                                    , SvgAttr.fill "currentColor"
                                    , SvgAttr.viewBox "0 0 20 20"
                                    ]
                                    [ path
                                        [ SvgAttr.fillRule "evenodd"
                                        , SvgAttr.d "M18 10a8 8 0 11-16 0 8 8 0 0116 0zm-7-4a1 1 0 11-2 0 1 1 0 012 0zM9 9a1 1 0 000 2v3a1 1 0 001 1h1a1 1 0 100-2v-3a1 1 0 00-1-1H9z"
                                        , SvgAttr.clipRule "evenodd"
                                        ]
                                        []
                                    ]
                                , text error
                                ]

                        Nothing ->
                            text ""
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
                                [ Html.div [] [ Html.h3 [] [ model.roomName |> Maybe.withDefault "Room name is not available" |> text ] ]
                                , Html.p [] [ model.stories |> List.head |> Maybe.withDefault "There are no stories" |> text ]
                                , Html.div []
                                    [ text "I am main content"
                                    , Html.div []
                                        [ if model.shouldShowCharts then
                                            viewCharts model

                                          else
                                            viewCards model
                                        ]
                                    ]
                                , Html.div []
                                    [ Html.div [] [ text <| Util.fromIntToCounter model.clock ]
                                    , text "I am sidebar. Copy link and send to collegues to invite"
                                    , Html.input [ Attr.readonly True, Attr.value <| model.url ++ "invite/" ++ (model.roomId |> Maybe.withDefault 1 |> String.fromInt) ] []
                                    , Html.div []
                                        [ text "Users:"
                                        , Html.ul []
                                            (model.users
                                                |> List.map
                                                    (\{ isAdmin, name, card, hasVoted } ->
                                                        if isAdmin then
                                                            Html.li [ Attr.css [ Tw.text_color Tw.blue_400 ] ]
                                                                [ Html.div []
                                                                    [ Html.p [] [ text name ]
                                                                    , case model.credentials of
                                                                        Admin ->
                                                                            case card of
                                                                                Just crd ->
                                                                                    Html.p [] [ crd |> String.fromFloat |> text ]

                                                                                Nothing ->
                                                                                    text ""

                                                                        Employee ->
                                                                            if model.shouldFlipCards then
                                                                                case card of
                                                                                    Just crd ->
                                                                                        Html.p [] [ crd |> String.fromFloat |> text ]

                                                                                    Nothing ->
                                                                                        text ""

                                                                            else
                                                                                text ""
                                                                    ]
                                                                ]

                                                        else
                                                            case model.credentials of
                                                                Admin ->
                                                                    Html.li []
                                                                        [ Html.div []
                                                                            [ Html.p [] [ text name ]
                                                                            , case card of
                                                                                Just crd ->
                                                                                    Html.p [] [ crd |> String.fromFloat |> text ]

                                                                                Nothing ->
                                                                                    text ""
                                                                            ]
                                                                        ]

                                                                Employee ->
                                                                    Html.li []
                                                                        [ Html.div []
                                                                            [ Html.p [] [ text name ]
                                                                            , if model.shouldFlipCards then
                                                                                case card of
                                                                                    Just crd ->
                                                                                        Html.p [] [ crd |> String.fromFloat |> text ]

                                                                                    Nothing ->
                                                                                        text ""

                                                                              else
                                                                                text ""
                                                                            ]
                                                                        , if hasVoted then
                                                                            Html.p [] [ text "📜" ]

                                                                          else
                                                                            text ""
                                                                        ]
                                                    )
                                            )
                                        ]
                                    ]
                                , Html.div []
                                    [ case model.credentials of
                                        Admin ->
                                            Html.div []
                                                [ if not <| model.shouldShowCharts && List.length model.stories > 0 then
                                                    if model.shouldStartClock then
                                                        Html.div []
                                                            [ Html.button [ onClick ResetTime ] [ text "Reset timer" ]
                                                            , Html.button [ onClick FlipCards ] [ text "Flip cards" ]
                                                            , Html.button [ onClick ClearVotes ] [ text "Clear votes" ]
                                                            , Html.button [ onClick NextStory ] [ text "Skip story" ]
                                                            , if model.users |> List.all (\user -> user.hasVoted) then
                                                                Html.button [ onClick FinishVoting ] [ text "Finish Voting" ]

                                                              else
                                                                text ""
                                                            ]

                                                    else
                                                        Html.button [ onClick StartTime ] [ text "Start timer" ]

                                                  else
                                                    text ""
                                                ]

                                        Employee ->
                                            text ""
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


viewCards : FrontendModel -> Html FrontendMsg
viewCards model =
    Html.ul []
        (cards
            |> List.map
                (\card ->
                    Html.li
                        (if model.shouldStartClock then
                            [ onClick <| ChooseCard card.value ]

                         else
                            []
                        )
                        [ Html.span [] [ text card.name ]
                        ]
                )
        )


viewCharts : FrontendModel -> Html FrontendMsg
viewCharts model =
    let
        teamSize =
            model.users |> List.length |> toFloat

        toChartData : Float -> List User -> List { uniqueVoteValue : Maybe Float, percentage : Float, numOfVoters : Float }
        toChartData count lst =
            case lst of
                [] ->
                    []

                x :: xs ->
                    let
                        listOfLeftValues =
                            xs |> List.map (\u -> u.card)
                    in
                    if List.isEmpty xs then
                        { numOfVoters = count, percentage = count / teamSize * 100, uniqueVoteValue = x.card } :: toChartData 1 []

                    else if List.member x.card listOfLeftValues then
                        [] ++ toChartData (count + 1) xs

                    else
                        { uniqueVoteValue = x.card, percentage = count / teamSize * 100, numOfVoters = count } :: toChartData 1 xs
    in
    Html.div []
        [ case model.credentials of
            Admin ->
                if List.isEmpty model.stories then
                    Html.p [] [ text "No more stories to estimate ! You are done !" ]

                else if model.shouldShowCharts then
                    Html.button [ onClick NextStory ] [ text "Next Story" ]

                else
                    text ""

            Employee ->
                text ""
        , Html.ul []
            (model.users
                |> List.sortBy
                    (\user ->
                        let
                            crd =
                                user.card |> Maybe.withDefault 0.5
                        in
                        crd
                    )
                |> toChartData 1
                |> List.map
                    (\entry ->
                        Html.li []
                            [ Html.div []
                                [ Html.div []
                                    [ Html.p [] [ entry.uniqueVoteValue |> Maybe.withDefault 0 |> String.fromFloat |> text ]
                                    , Html.p [] [ text <| (entry.percentage |> String.fromFloat) ++ "%" ]
                                    , Html.p [] [ text <| "(" ++ (entry.numOfVoters |> String.fromFloat) ++ " players)" ]
                                    ]

                                --  TODO PIE CHART placeholder ! , Chart.pie [ ( 1, "Dsuan" ), ( 2, "Pera" ), ( 3, "Bojana" ), ( 1, "Vaja" ), ( 2, "Peka" ) ] |> Chart.toHtml
                                ]
                            ]
                    )
            )
        ]
