module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Css
import Css.Global
import Donut
import Html.Styled as Html exposing (Html, text)
import Html.Styled.Attributes as Attr
import Html.Styled.Events exposing (onClick, onInput)
import Lamdera exposing (sendToBackend)
import Process
import Svg.Styled exposing (g, path, svg)
import Svg.Styled.Attributes as SvgAttr
import Tailwind.Breakpoints as Bp
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
            Ok trimmedStr


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
    , url = Util.getBaseUrl url
    , status = EnterAdminNameStep
    , name = Nothing
    , roomName = Nothing
    , story = Nothing
    , error = Nothing
    , roomId = Nothing
    , stories = []
    , credentials = Admin
    , users = []
    , sessionId = Nothing
    , clock = 0
    , chart = Bar
    , shouldStartClock = False
    , shouldFlipCards = False
    , shouldShowCharts = False
    , shouldStartChartAnimation = False
    , card = Nothing
    , announcement = []
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
            , sendToBackend <| ReqRoomRoute roomId Employee
            )

        RoomRoute _ ->
            let
                mdl =
                    initialModel url key
            in
            ( mdl, Nav.replaceUrl key "/" )


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
                    ( model, sendToBackend <| ReqRoomRoute roomId Employee )

                RoomRoute roomId ->
                    ( { model | status = PokerStep }, sendToBackend <| ReqRoomRoute roomId Admin )

                NotFound ->
                    ( model, Cmd.none )

        SendName cred ->
            case validateInput model.name of
                Err errorMessage ->
                    ( { model | error = errorMessage }, Cmd.none )

                Ok validInput ->
                    case cred of
                        Admin ->
                            -- TODO fix sessionId not use maybe
                            ( { model | status = CreateRoomStep, error = Nothing, name = Nothing, users = { defaultUser | sessionId = model.sessionId |> Maybe.withDefault "123", name = validInput, isAdmin = True } :: model.users }
                            , sendToBackend <| SendAdminNameToBE validInput
                            )

                        Employee ->
                            case model.sessionId of
                                Just sessionId ->
                                    ( { model | status = PokerStep, error = Nothing, name = Nothing, users = { defaultUser | sessionId = sessionId, name = validInput } :: model.users }
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
                        -- TODO think about something smarter
                        roomId =
                            model.roomId |> Maybe.withDefault 1

                        updatedStories =
                            model.stories ++ [ validInput ]
                    in
                    ( { model | error = Nothing, status = PokerStep, story = Nothing, stories = updatedStories }
                    , Cmd.batch [ sendToBackend <| SendStoryToBE updatedStories roomId, Nav.pushUrl model.key <| "/room/" ++ (roomId |> String.fromInt) ]
                    )

        StoreName str ->
            ( { model | name = Just str, error = Nothing }, Cmd.none )

        StoreRoom str ->
            ( { model | roomName = Just str, error = Nothing }, Cmd.none )

        StoreStory str ->
            ( { model | story = Just str, error = Nothing }, Cmd.none )

        ChooseCard cardValue cardName ->
            case model.sessionId of
                Just justSessionId ->
                    let
                        updatedUsers =
                            model.users
                                |> List.map
                                    (\user ->
                                        if user.sessionId == justSessionId then
                                            { user | card = Just cardValue, voteState = HiddenVote }

                                        else
                                            user
                                    )
                    in
                    ( { model | users = updatedUsers, card = Just cardName }, sendToBackend <| SendCard cardValue (model.roomId |> Maybe.withDefault 1) )

                Nothing ->
                    ( model, Cmd.none )

        Tick _ ->
            ( { model | clock = model.clock + 1 }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        StartTime ->
            ( { model | shouldStartClock = True }, sendToBackend <| StartTimerAndVote (model.roomId |> Maybe.withDefault 1) )

        ResetTime ->
            ( { model | shouldStartClock = False, clock = 0 }, sendToBackend <| ResetTimerAndVote (model.roomId |> Maybe.withDefault 1) )

        ShowCards ->
            ( model, sendToBackend <| InitiateShowCards (model.roomId |> Maybe.withDefault 1) )

        HideCards ->
            ( model, sendToBackend <| InitiateHideCards (model.roomId |> Maybe.withDefault 1) )

        ClearVotes ->
            ( { model | shouldFlipCards = False, card = Nothing }, sendToBackend <| ClearAllUserVotes (model.roomId |> Maybe.withDefault 1) )

        FinishVoting ->
            ( { model | shouldStartClock = False }
            , Cmd.batch
                [ sendToBackend <| SignalShowCharts (model.roomId |> Maybe.withDefault 1)
                , Process.sleep 500
                    |> Task.perform (\_ -> StartChartAnimation)
                ]
            )

        SkipStory ->
            ( { model | shouldStartClock = False, clock = 0, card = Nothing, shouldFlipCards = False, shouldShowCharts = False }, sendToBackend <| SignalSkipStory (model.roomId |> Maybe.withDefault 1) )

        NextStory ->
            let
                updatedStories =
                    model.stories |> List.drop 1
            in
            ( { model | stories = updatedStories, card = Nothing, shouldStartChartAnimation = False }
            , Cmd.batch
                [ sendToBackend <| ClearAllUserVotes (model.roomId |> Maybe.withDefault 1)
                , sendToBackend <| SignalUpdateStories updatedStories (model.roomId |> Maybe.withDefault 1)
                ]
            )

        StartChartAnimation ->
            ( { model | shouldStartChartAnimation = True }, sendToBackend <| SignalChartAnimation (model.roomId |> Maybe.withDefault 1) )

        ShowDonutChart ->
            ( { model | chart = Donut }, Cmd.none )

        ShowBarChart ->
            ( { model | chart = Bar }, Cmd.none )

        HideNotification ->
            ( { model | announcement = List.drop 1 model.announcement }, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        SendRoomIdToFE roomId ->
            ( { model | roomId = Just roomId }, Cmd.none )

        ResRoomRoute { status, roomName, sessionId, stories, users } ->
            let
                isAdmin =
                    users
                        |> List.filter (\user -> user.sessionId == sessionId)
                        |> List.head
                        |> Maybe.withDefault defaultUser
                        |> .isAdmin
            in
            ( { model
                | status = status
                , roomName = Just roomName
                , sessionId = Just sessionId
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

        UpdateRoom { sessionId, name } ->
            let
                updatedAnnouncement =
                    model.announcement ++ [ name ++ " has joined !" ]
            in
            ( { model | users = { defaultUser | sessionId = sessionId, name = name } :: model.users, announcement = updatedAnnouncement }
            , Process.sleep 4000
                |> Task.perform (\_ -> HideNotification)
            )

        SupplyBEData { stories, users } ->
            ( { model | stories = stories, users = users }, Cmd.none )

        UsersStartTimer ->
            let
                updatedAnnouncement =
                    model.announcement
                        |> (++) [ "Start voting !" ]
            in
            ( { model | shouldStartClock = True, announcement = updatedAnnouncement }
            , Process.sleep 4000
                |> Task.perform (\_ -> HideNotification)
            )

        UsersResetTimer ->
            ( { model | shouldStartClock = False, clock = 0, announcement = [] }, Cmd.none )

        UpdateCards users ->
            ( { model | users = users }, Cmd.none )

        UsersFlipCards users ->
            ( { model | users = users, shouldFlipCards = not <| model.shouldFlipCards }, Cmd.none )

        UsersCardReset users ->
            ( { model | users = users, shouldFlipCards = False, card = Nothing }, Cmd.none )

        SkipStoryAndExposeCharts users ->
            ( { model | users = users, shouldFlipCards = False, card = Nothing, shouldShowCharts = True, shouldStartClock = False, clock = 0, announcement = [] }, Cmd.none )

        ExposeCharts ->
            ( { model | shouldShowCharts = not <| model.shouldShowCharts, shouldStartClock = False, clock = 0, announcement = [] }, Cmd.none )

        UpdateStories updatedStories resetUsers ->
            ( { model | stories = updatedStories, shouldShowCharts = not <| model.shouldShowCharts, users = resetUsers, shouldStartChartAnimation = False }, Cmd.none )

        ChartAnimation ->
            ( { model | shouldStartChartAnimation = True }, Cmd.none )


view : Model -> Browser.Document FrontendMsg
view model =
    { title = "EstPoker"
    , body =
        [ Html.toUnstyled <|
            Html.div [ Attr.css [ Tw.flex, Tw.h_full, Tw.bg_color Tw.black, Tw.h_screen ] ]
                [ Html.div [ Attr.css [ Tw.h_full, Tw.w_full, Tw.flex, Tw.flex_col ] ]
                    [ viewNotifications model
                    , case model.status of
                        EnterAdminNameStep ->
                            Html.div
                                [ Attr.css
                                    [ Tw.flex
                                    , Tw.min_h_full
                                    , Tw.flex_col
                                    , Tw.justify_center
                                    , Tw.px_6
                                    , Tw.text_center
                                    , Bp.lg
                                        [ Tw.px_8
                                        ]
                                    ]
                                ]
                                [ Html.div
                                    [ Attr.css
                                        [ Tw.text_color Tw.white
                                        , Tw.text_2xl
                                        , Bp.sm
                                            [ Tw.mx_auto
                                            , Tw.w_full
                                            , Tw.max_w_sm
                                            ]
                                        ]
                                    ]
                                    [ Html.h2 [ Attr.css [ Tw.mt_0, Tw.mb_4 ] ] [ text "Add your name" ] ]
                                , Html.div
                                    [ Attr.css
                                        [ Bp.sm
                                            [ Tw.mx_auto
                                            , Tw.w_full
                                            , Tw.max_w_sm
                                            ]
                                        ]
                                    ]
                                    [ Html.p [ Attr.css [ Tw.text_color Tw.gray_400, Tw.text_lg, Tw.italic, Tw.mb_4, Tw.mt_0, Tw.font_extralight ] ] [ text "[ You're about to become an admin ]" ]
                                    , inputStyle
                                        |> withError model.error
                                        |> viewInput StoreName
                                            (model.name
                                                |> Maybe.withDefault ""
                                            )
                                    ]
                                , Html.div
                                    [ Attr.css
                                        [ Tw.mt_5
                                        , Bp.sm
                                            [ Tw.mx_auto
                                            , Tw.w_full
                                            , Tw.max_w_sm
                                            ]
                                        ]
                                    ]
                                    [ viewButtonWithMsg (SendName Admin) "Save" ]
                                ]

                        EnterNameStep ->
                            Html.div
                                [ Attr.css
                                    [ Tw.flex
                                    , Tw.min_h_full
                                    , Tw.flex_col
                                    , Tw.text_color Tw.white
                                    , Tw.justify_center
                                    , Tw.px_6
                                    , Tw.text_center
                                    , Bp.lg
                                        [ Tw.px_8
                                        ]
                                    ]
                                ]
                                [ Html.div
                                    [ Attr.css
                                        [ Tw.text_2xl
                                        , Bp.sm
                                            [ Tw.mx_auto
                                            , Tw.w_full
                                            , Tw.max_w_sm
                                            ]
                                        ]
                                    ]
                                    [ Html.p [] [ text "Hello, you've been invited to" ]
                                    , Html.h2 [] [ model.roomName |> Maybe.withDefault "Room name not available" |> text ]
                                    ]
                                , Html.div
                                    [ Attr.css
                                        [ Bp.sm
                                            [ Tw.mx_auto
                                            , Tw.w_full
                                            , Tw.max_w_sm
                                            ]
                                        ]
                                    ]
                                    [ Html.p [ Attr.css [ Tw.mt_0, Tw.mb_4, Tw.text_2xl ] ] [ text "Add your name" ]
                                    , Html.p [ Attr.css [ Tw.text_color Tw.gray_400, Tw.text_lg, Tw.italic, Tw.mb_4, Tw.mt_0, Tw.font_extralight ] ] [ text "[ After that we will redirect you to team's room ]" ]
                                    , inputStyle
                                        |> withError model.error
                                        |> viewInput StoreName
                                            (model.name
                                                |> Maybe.withDefault ""
                                            )
                                    ]
                                , Html.div
                                    [ Attr.css
                                        [ Tw.mt_5
                                        , Bp.sm
                                            [ Tw.mx_auto
                                            , Tw.w_full
                                            , Tw.max_w_sm
                                            ]
                                        ]
                                    ]
                                    [ viewButtonWithMsg (SendName Employee) "Save" ]
                                ]

                        CreateRoomStep ->
                            Html.div
                                [ Attr.css
                                    [ Tw.flex
                                    , Tw.min_h_full
                                    , Tw.flex_col
                                    , Tw.justify_center
                                    , Tw.px_6
                                    , Tw.text_center
                                    , Bp.lg
                                        [ Tw.px_8
                                        ]
                                    ]
                                ]
                                [ Html.div
                                    [ Attr.css
                                        [ Tw.text_color Tw.white
                                        , Tw.text_2xl
                                        , Bp.sm
                                            [ Tw.mx_auto
                                            , Tw.w_full
                                            , Tw.max_w_sm
                                            ]
                                        ]
                                    ]
                                    [ Html.h2 [ Attr.css [ Tw.mt_0, Tw.mb_4 ] ] [ text "Create new room" ] ]
                                , Html.div
                                    [ Attr.css
                                        [ Bp.sm
                                            [ Tw.mx_auto
                                            , Tw.w_full
                                            , Tw.max_w_sm
                                            ]
                                        ]
                                    ]
                                    [ Html.p [ Attr.css [ Tw.text_color Tw.gray_400, Tw.text_lg, Tw.italic, Tw.mb_4, Tw.mt_0, Tw.font_extralight ] ] [ text "[ Place where you can vote for stories ]" ]
                                    , inputStyle
                                        |> withError model.error
                                        |> viewInput StoreRoom
                                            (model.roomName
                                                |> Maybe.withDefault ""
                                            )
                                    ]
                                , Html.div
                                    [ Attr.css
                                        [ Tw.mt_5
                                        , Bp.sm
                                            [ Tw.mx_auto
                                            , Tw.w_full
                                            , Tw.max_w_sm
                                            ]
                                        ]
                                    ]
                                    [ viewButtonWithMsg SendRoom "Create" ]
                                ]

                        CreateStoryStep ->
                            Html.div
                                [ Attr.css
                                    [ Tw.text_color Tw.white
                                    , Tw.flex
                                    , Tw.min_h_full
                                    , Tw.flex_col
                                    , Tw.justify_center
                                    , Tw.px_6
                                    , Tw.text_center
                                    , Bp.lg
                                        [ Tw.px_8
                                        ]
                                    ]
                                ]
                                [ Html.div
                                    [ Attr.css
                                        [ Tw.text_2xl
                                        , Bp.sm
                                            [ Tw.mx_auto
                                            , Tw.w_full
                                            , Tw.max_w_sm
                                            ]
                                        ]
                                    ]
                                    [ Html.h2 [ Attr.css [ Tw.mt_0, Tw.mb_4 ] ] [ text "Create new story" ] ]
                                , Html.div
                                    [ Attr.css
                                        [ Bp.sm
                                            [ Tw.mx_auto
                                            , Tw.w_full
                                            , Tw.max_w_sm
                                            ]
                                        ]
                                    ]
                                    [ Html.p [ Attr.css [ Tw.text_color Tw.gray_400, Tw.text_lg, Tw.italic, Tw.mb_4, Tw.mt_0, Tw.font_extralight ] ] [ text "[ Add multiple or one story ]" ]
                                    , Html.div [ Attr.css [ Tw.relative, Tw.mx_auto ] ]
                                        [ Html.ul [ Attr.css [ Tw.list_none, Tw.flex, Tw.p_0, Tw.m_0, Tw.text_2xl, Tw.gap_4, Tw.absolute, Tw.w_full ] ]
                                            (model.stories
                                                |> List.map
                                                    (\story ->
                                                        Html.li
                                                            [ Attr.css
                                                                [ Tw.transition_all
                                                                , Tw.absolute
                                                                , Tw.w_full
                                                                ]
                                                            , Attr.class
                                                                "hide-after-n"
                                                            ]
                                                            [ text story ]
                                                    )
                                            )
                                        ]
                                    , inputStyle
                                        |> withError model.error
                                        |> viewInput StoreStory
                                            (model.story
                                                |> Maybe.withDefault ""
                                            )
                                    ]
                                , Html.div
                                    [ Attr.css
                                        [ Tw.mt_5
                                        , Tw.mx_auto
                                        , Tw.w_full
                                        , Tw.max_w_sm
                                        , Tw.flex
                                        , Tw.gap_6
                                        , Tw.justify_center
                                        ]
                                    ]
                                    [ viewButtonWithMsg SendStory "Add New"
                                    , viewButtonWithMsg SaveStory "Save"
                                    ]
                                ]

                        PokerStep ->
                            Html.div
                                [ Attr.css
                                    [ Tw.px_6
                                    , Tw.flex
                                    , Tw.flex_col
                                    , Tw.text_color Tw.white
                                    , Tw.max_w_7xl
                                    , Tw.m_auto
                                    , Bp.lg
                                        [ Tw.flex_row
                                        ]
                                    ]
                                ]
                                [ Html.div
                                    [ Attr.css
                                        [ Tw.flex
                                        , Tw.flex_col
                                        , Tw.flex_1
                                        , Tw.py_6
                                        , Bp.lg
                                            [ Tw.pr_4
                                            ]
                                        ]
                                    ]
                                    [ Html.div
                                        [ Attr.css
                                            [ Tw.text_color Tw.white
                                            , Tw.text_2xl
                                            , Tw.flex
                                            , Tw.flex_row
                                            , Tw.justify_between
                                            , Tw.flex_col
                                            , Tw.gap_6
                                            , Bp.sm
                                                [ Tw.gap_10, Tw.items_center, Tw.flex_row ]
                                            ]
                                        ]
                                        [ Html.h2 [ Attr.css [ Tw.m_0, Tw.break_all ] ] [ model.roomName |> Maybe.withDefault "Room name is not available" |> text ]
                                        , if model.shouldShowCharts then
                                            Html.div [ Attr.css [ Tw.flex, Tw.gap_2, Tw.text_xl ] ]
                                                [ Html.span
                                                    [ case model.chart of
                                                        Bar ->
                                                            Attr.css [ Tw.text_color Tw.gray_400 ]

                                                        Donut ->
                                                            Attr.css
                                                                [ Tw.text_color Tw.white
                                                                , Tw.cursor_pointer
                                                                ]
                                                    , onClick ShowBarChart
                                                    ]
                                                    [ text "Bar Chart" ]
                                                , Html.span [] [ text " | " ]
                                                , Html.span
                                                    [ case model.chart of
                                                        Donut ->
                                                            Attr.css [ Tw.text_color Tw.gray_400 ]

                                                        Bar ->
                                                            Attr.css
                                                                [ Tw.text_color Tw.white
                                                                , Tw.cursor_pointer
                                                                ]
                                                    , onClick ShowDonutChart
                                                    ]
                                                    [ text "Donut Chart" ]
                                                ]

                                          else
                                            text ""
                                        ]
                                    , Html.h4 [ Attr.css [ Tw.text_2xl, Tw.text_color Tw.gray_400, Tw.font_extralight, Tw.break_all ] ] [ model.stories |> List.head |> Maybe.withDefault "There are no more stories" |> (++) "[ Current story ] " |> text ]
                                    , Html.div []
                                        [ Html.div []
                                            [ if model.shouldShowCharts then
                                                viewCharts model

                                              else
                                                viewCards model
                                            ]
                                        ]
                                    , Html.div [ Attr.css [ Tw.mt_10 ] ]
                                        [ case model.credentials of
                                            Admin ->
                                                Html.div
                                                    [ Attr.css
                                                        [ Tw.flex
                                                        , Tw.gap_4
                                                        , Tw.flex_wrap
                                                        , Tw.justify_center
                                                        , Bp.lg [ Tw.justify_start ]
                                                        ]
                                                    ]
                                                    [ if not <| model.shouldShowCharts && List.length model.stories > 0 then
                                                        if model.shouldStartClock then
                                                            Html.div
                                                                [ Attr.css
                                                                    [ Tw.flex
                                                                    , Tw.gap_4
                                                                    , Tw.flex_wrap
                                                                    , Tw.justify_center
                                                                    , Bp.lg [ Tw.justify_start ]
                                                                    ]
                                                                ]
                                                                [ viewButtonWithMsg ResetTime "Reset timer"
                                                                , if model.shouldFlipCards then
                                                                    viewButtonWithMsg HideCards "Hide Votes "

                                                                  else
                                                                    viewButtonWithMsg ShowCards "Show votes"
                                                                , viewButtonWithMsg ClearVotes "Clear votes"
                                                                , viewButtonWithMsg SkipStory "Skip story"
                                                                , if model.users |> List.all (\user -> not <| user.voteState == NotVoted) then
                                                                    viewButtonWithMsg FinishVoting "Finish Voting"

                                                                  else
                                                                    text ""
                                                                ]

                                                        else
                                                            viewButtonWithMsg StartTime "Start timer"

                                                      else
                                                        text ""
                                                    ]

                                            Employee ->
                                                text ""
                                        ]
                                    , Html.div [ Attr.css [ Tw.mt_10 ] ]
                                        [ Html.h4 [ Attr.css [ Tw.text_3xl, Tw.m_0, Tw.mb_4 ] ] [ text "Stories:" ]
                                        ]
                                    , Html.ul [ Attr.css [ Tw.list_none, Tw.flex, Tw.p_0, Tw.m_0, Tw.flex_col, Tw.text_2xl, Tw.gap_2 ] ]
                                        (model.stories
                                            |> List.map
                                                (\story ->
                                                    Html.li [ Attr.css [ Tw.break_all ] ] [ text story ]
                                                )
                                        )
                                    ]
                                , Html.div
                                    [ Attr.css
                                        [ Tw.flex
                                        , Tw.items_center
                                        , Tw.py_6
                                        , Bp.lg
                                            [ Tw.pl_10
                                            , Tw.text_right
                                            , Tw.border_l
                                            , Tw.border_color Tw.gray_600
                                            , Tw.border_solid
                                            , Tw.border_r_0
                                            , Tw.border_b_0
                                            , Tw.border_t_0
                                            ]
                                        ]
                                    ]
                                    [ Html.div [ Attr.css [ Tw.flex, Tw.flex_col ] ]
                                        [ Html.div [ Attr.css [ Tw.text_5xl ] ] [ text <| Util.fromIntToCounter model.clock ]
                                        , Html.p [ Attr.css [ Tw.text_2xl, Tw.text_color Tw.gray_400, Tw.font_extralight ] ] [ text "[ Copy link and send to collegues ]" ]
                                        , Html.div []
                                            [ Html.input
                                                [ Attr.readonly True
                                                , Attr.css
                                                    [ Tw.block
                                                    , Tw.w_full
                                                    , Tw.form_input
                                                    , Tw.rounded_md
                                                    , Tw.border_0
                                                    , Tw.py_2
                                                    , Tw.pl_3
                                                    , Tw.pr_3
                                                    , Tw.shadow_sm
                                                    , Tw.ring_1
                                                    , Tw.ring_inset
                                                    , Tw.ring_color Tw.gray_300
                                                    , Tw.bg_color Tw.slate_900
                                                    , Tw.font_mono
                                                    , Tw.text_color Tw.teal_400
                                                    , Css.focus
                                                        [ Tw.outline_0
                                                        , Tw.ring_color Tw.gray_300
                                                        ]
                                                    , Bp.sm
                                                        [ Tw.text_lg
                                                        , Tw.leading_6
                                                        ]
                                                    , Bp.lg [ Tw.text_right ]
                                                    ]
                                                , Attr.value <| model.url ++ "invite/" ++ (model.roomId |> Maybe.withDefault 1 |> String.fromInt)
                                                ]
                                                []
                                            ]
                                        , Html.div [ Attr.css [ Tw.mt_6 ] ]
                                            [ Html.h4 [ Attr.css [ Tw.text_3xl, Tw.m_0, Tw.mb_4 ] ] [ text "Team:" ]
                                            , Html.ul [ Attr.css [ Tw.list_none, Tw.flex, Tw.p_0, Tw.m_0, Tw.flex_col, Tw.text_2xl, Tw.gap_4 ] ]
                                                (model.users
                                                    |> List.map
                                                        (\{ isAdmin, name, card, voteState } ->
                                                            if isAdmin then
                                                                Html.li [ Attr.css [ Tw.flex, Tw.justify_end, Tw.gap_4, Tw.text_color Tw.blue_400, Tw.break_all ] ]
                                                                    (viewNameCardGroup
                                                                        { voteState = voteState, card = card, name = name }
                                                                    )

                                                            else
                                                                Html.li [ Attr.css [ Tw.flex, Tw.justify_end, Tw.gap_4, Tw.break_all ] ]
                                                                    (viewNameCardGroup
                                                                        { voteState = voteState, card = card, name = name }
                                                                    )
                                                        )
                                                )
                                            ]
                                        ]
                                    ]
                                ]

                        Step404 ->
                            Html.div
                                [ Attr.css
                                    [ Tw.px_6
                                    , Tw.flex
                                    , Tw.flex_row
                                    , Tw.text_color Tw.white
                                    , Tw.max_w_7xl
                                    , Tw.m_auto
                                    ]
                                ]
                                [ Html.div
                                    [ Attr.css
                                        [ Tw.text_color Tw.white
                                        , Tw.text_2xl
                                        , Bp.sm
                                            [ Tw.mx_auto
                                            , Tw.w_full
                                            , Tw.max_w_sm
                                            ]
                                        ]
                                    ]
                                    [ Html.h2 [ Attr.css [ Tw.mt_0, Tw.mb_4 ] ] [ text "4 ō 4 bud..." ]
                                    , Html.p [] [ text "Nothing to see here -_-" ]
                                    , Html.a [ Attr.css [ Tw.text_xl, Tw.text_color Tw.teal_400 ], Attr.href "/" ] [ text "Go Home" ]
                                    ]
                                ]
                    ]
                ]
        ]
    }


viewNameCardGroup : { voteState : VoteState, card : Maybe Float, name : ValidTextField } -> List (Html FrontendMsg)
viewNameCardGroup { voteState, card, name } =
    [ Html.div []
        [ case voteState of
            HiddenVote ->
                Html.span [ Attr.css [ Tw.m_0 ] ] [ 0xA936 |> Char.fromCode |> String.fromChar |> text ]

            NotVoted ->
                text ""

            Voted ->
                case card of
                    Just crd ->
                        Html.span [ Attr.css [] ] [ crd |> String.fromFloat |> text ]

                    Nothing ->
                        text ""
        ]
    , Html.p [ Attr.css [ Tw.m_0 ] ] [ text name ]
    ]


viewCards : FrontendModel -> Html FrontendMsg
viewCards model =
    Html.div []
        [ Html.h3 [ Attr.css [ Tw.text_color Tw.gray_400, Tw.font_extralight ] ] [ text "[ Pick card to estimate story ]" ]
        , Html.ul
            [ Attr.css
                [ Tw.list_none
                , Tw.flex
                , Tw.flex_wrap
                , Tw.p_0
                , Tw.m_0
                , Tw.text_2xl
                , Bp.md
                    [ Tw.gap_10 ]
                ]
            ]
            (Util.cards
                |> List.map
                    (\card ->
                        Html.li
                            [ Attr.css
                                [ Tw.flex
                                , Tw.border_color Tw.white
                                , Tw.border_solid
                                , Tw.border_2
                                , Tw.justify_center
                                , Tw.w_1over3
                                , Tw.transition_all
                                , Tw.duration_300
                                , Tw.relative
                                , Tw.cursor_pointer
                                , Bp.md
                                    [ Tw.w_1over4 ]
                                ]
                            , if model.shouldStartClock then
                                let
                                    crd =
                                        model.card |> Maybe.withDefault "0"
                                in
                                if crd /= card.name then
                                    onClick (ChooseCard card.value card.name)

                                else
                                    onClick NoOp

                              else
                                Attr.css []
                            , case model.card of
                                Just crd ->
                                    if crd == card.name then
                                        Attr.css
                                            [ Tw.bg_color Tw.white
                                            , Tw.border_color Tw.white
                                            , Tw.text_color Tw.black
                                            , Tw.rounded
                                            , Tw.font_bold
                                            , Tw.text_7xl
                                            , Tw.p_0
                                            , Css.hover
                                                [ Tw.p_0
                                                ]
                                            ]

                                    else
                                        Attr.css
                                            [ Tw.p_5
                                            , Css.hover
                                                [ Tw.bg_color Tw.white
                                                , Tw.border_color Tw.white
                                                , Tw.text_color Tw.black
                                                , Tw.rounded
                                                , Tw.font_bold
                                                , Tw.text_7xl
                                                , Tw.p_0
                                                ]
                                            ]

                                Nothing ->
                                    Attr.css
                                        [ Tw.flex
                                        , Tw.border_color Tw.white
                                        , Tw.border_solid
                                        , Tw.border_2
                                        , Tw.justify_center
                                        , Tw.w_1over4
                                        , Tw.transition_all
                                        , Tw.duration_300
                                        , Tw.relative
                                        , Tw.p_5
                                        , Css.hover
                                            [ Tw.bg_color Tw.white
                                            , Tw.border_color Tw.white
                                            , Tw.text_color Tw.black
                                            , Tw.rounded
                                            , Tw.font_bold
                                            , Tw.text_7xl
                                            , Tw.p_0
                                            ]
                                        ]
                            ]
                            [ Html.span [] [ text card.name ]
                            ]
                    )
            )
        ]


viewCharts : FrontendModel -> Html FrontendMsg
viewCharts model =
    let
        teamSize =
            model.users |> List.length |> toFloat
    in
    Html.div []
        [ case model.card of
            Just _ ->
                case model.chart of
                    Bar ->
                        Html.div []
                            [ Html.ul [ Attr.css [ Tw.list_none, Tw.flex, Tw.flex_col, Tw.p_0, Tw.m_0, Tw.text_2xl, Tw.mb_10, Tw.gap_4 ] ]
                                (model.users
                                    |> List.sortBy
                                        (\user ->
                                            let
                                                crd =
                                                    user.card |> Maybe.withDefault 0.5
                                            in
                                            crd
                                        )
                                    |> Util.toChartData 1 teamSize
                                    |> List.indexedMap
                                        (\int entry ->
                                            Html.li []
                                                [ Html.div [ Attr.css [ Tw.flex, Tw.flex_col, Tw.gap_4, Tw.justify_end, Bp.lg [ Tw.flex_row ] ] ]
                                                    [ Html.div [ Attr.css [ Tw.flex, Tw.gap_4 ] ]
                                                        [ Html.span [ Attr.css [ Css.width (Css.px 31), Css.height (Css.px 31), Tw.bg_color (Util.getColor int), Tw.hidden, Bp.lg [ Tw.block ] ] ] []
                                                        , Html.span [ Attr.css [ Css.minWidth (Css.px 40) ] ] [ entry.uniqueVoteValue |> Maybe.withDefault 0 |> String.fromFloat |> text ]
                                                        , Html.span [ Attr.css [ Css.minWidth (Css.px 57) ] ] [ text <| Util.roundFloat entry.percentage 2 ++ "%" ]
                                                        , Html.span [ Attr.css [ Css.minWidth (Css.px 123) ] ] [ text <| "(" ++ (entry.numOfVoters |> String.fromFloat) ++ " " ++ pluralification entry.numOfVoters "player" ++ ")" ]
                                                        ]
                                                    , Html.div [ Attr.css [ Tw.w_full, Tw.bg_color Tw.slate_900, Tw.h_8, Bp.lg [ Tw.w_72 ] ] ]
                                                        [ Html.div
                                                            [ if model.shouldStartChartAnimation then
                                                                Attr.class "transition-width"

                                                              else
                                                                Attr.css []
                                                            , Attr.css
                                                                [ Tw.h_full
                                                                , Tw.bg_color (Util.getColor int)
                                                                , if model.shouldStartChartAnimation then
                                                                    Css.width (Css.pct entry.percentage)

                                                                  else
                                                                    Css.width (Css.px 0)
                                                                ]
                                                            ]
                                                            []
                                                        ]
                                                    ]
                                                ]
                                        )
                                )
                            ]

                    Donut ->
                        let
                            donutModel =
                                Donut.init model.users
                        in
                        Html.div [ Attr.css [ Tw.flex, Tw.flex_1, Tw.flex_col, Bp.lg [ Tw.flex_row ] ] ]
                            [ Html.div []
                                [ Html.ul [ Attr.css [ Tw.list_none, Tw.flex, Tw.flex_col, Tw.p_0, Tw.m_0, Tw.text_2xl, Tw.mb_10, Tw.gap_4 ] ]
                                    (model.users
                                        |> List.sortBy
                                            (\user ->
                                                let
                                                    crd =
                                                        user.card |> Maybe.withDefault 0.5
                                                in
                                                crd
                                            )
                                        |> Util.toChartData 1 teamSize
                                        |> List.indexedMap
                                            (\int entry ->
                                                Html.li []
                                                    [ Html.div [ Attr.css [ Tw.flex, Tw.gap_4, Bp.lg [ Tw.justify_end ] ] ]
                                                        [ Html.div [ Attr.css [ Tw.flex, Tw.gap_4 ] ]
                                                            [ Html.span [ Attr.css [ Css.width (Css.px 31), Css.height (Css.px 31), Tw.bg_color (Util.getColor int) ] ] []
                                                            , Html.span [ Attr.css [ Css.minWidth (Css.px 40) ] ] [ entry.uniqueVoteValue |> Maybe.withDefault 0 |> String.fromFloat |> text ]
                                                            , Html.span [ Attr.css [ Css.minWidth (Css.px 57) ] ] [ text <| Util.roundFloat entry.percentage 2 ++ "%" ]
                                                            , Html.span [ Attr.css [ Css.minWidth (Css.px 123) ] ] [ text <| "(" ++ (entry.numOfVoters |> String.fromFloat) ++ " " ++ pluralification entry.numOfVoters "player" ++ ")" ]
                                                            ]
                                                        ]
                                                    ]
                                            )
                                    )
                                ]
                            , Donut.view donutModel
                            ]

            Nothing ->
                Html.h4 [] [ text "This story was skipped" ]
        , if model.shouldShowCharts && List.length model.stories > 1 && model.credentials == Admin then
            viewButtonWithMsg NextStory "Next Story"

          else
            text ""
        ]


buttonStyle : List Css.Style
buttonStyle =
    [ Tw.bg_color Tw.teal_400
    , Tw.text_color Tw.white
    , Tw.py_1
    , Tw.px_4
    , Tw.text_xl
    , Tw.border
    , Tw.border_color Tw.teal_400
    , Tw.rounded
    , Tw.cursor_pointer
    , Tw.transition_all
    , Css.hover
        [ Tw.bg_color Tw.teal_700
        , Tw.border_color Tw.teal_400
        , Tw.border_color Tw.transparent
        ]
    ]


pluralification : Float -> String -> String
pluralification count initial =
    if count == 1 then
        initial

    else
        initial ++ "s"


inputStyle : List Css.Style
inputStyle =
    [ Tw.block
    , Tw.w_full
    , Tw.form_input
    , Tw.rounded_md
    , Tw.border_0
    , Tw.py_1_dot_5
    , Tw.h_10
    , Tw.text_color Tw.gray_900
    , Tw.shadow_sm
    , Tw.ring_1
    , Tw.ring_inset
    , Tw.ring_color Tw.gray_300
    , Css.focus
        [ Tw.ring_2
        , Tw.ring_inset
        , Tw.ring_color Tw.teal_400
        ]
    , Bp.sm
        [ Tw.text_lg
        , Tw.leading_6
        ]
    ]


withError : InvalidTextFiled -> List Css.Style -> List Css.Style
withError maybeError basicStyle =
    case maybeError of
        Just _ ->
            basicStyle ++ [ Tw.bg_color Tw.red_200 ]

        Nothing ->
            basicStyle


viewInput : (String -> FrontendMsg) -> String -> List Css.Style -> Html FrontendMsg
viewInput toMsg value styles =
    Html.input
        [ Attr.type_ "text"
        , Attr.css styles
        , onInput toMsg
        , Attr.value value
        ]
        []


viewButtonWithMsg : FrontendMsg -> String -> Html FrontendMsg
viewButtonWithMsg msg label =
    Html.button [ Attr.css buttonStyle, onClick msg ] [ text label ]


viewNotifications : Model -> Html FrontendMsg
viewNotifications { error, announcement } =
    Html.div [ Attr.css [ Tw.absolute, Tw.w_full ] ]
        [ Html.div
            [ Attr.css
                [ Tw.p_2
                , Tw.w_full
                , Tw.flex
                , case error of
                    Just _ ->
                        Tw.flex

                    Nothing ->
                        Tw.hidden
                , Tw.text_lg
                , Tw.justify_center
                , Tw.text_color Tw.red_800
                , Tw.bg_color Tw.red_200
                ]
            ]
            [ svg
                [ Attr.css
                    [ Tw.flex_shrink_0
                    , Tw.inline
                    , Tw.w_7
                    , Tw.h_7
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
            , case error of
                Just err ->
                    text err

                Nothing ->
                    text ""
            ]
        , Html.ul
            [ Attr.css
                [ Tw.list_none
                , Tw.flex
                , Tw.p_0
                , Tw.m_0
                , Tw.flex_col
                , Tw.fixed
                , Tw.right_5
                , Tw.z_10
                , Tw.top_5
                , Tw.overflow_hidden
                ]
            ]
            (announcement
                |> List.map
                    (\info ->
                        Html.li
                            [ Attr.css
                                [ Tw.p_2
                                , Tw.rounded
                                , Tw.flex
                                , Tw.text_lg
                                , Tw.justify_center
                                , Tw.text_color Tw.green_800
                                , Tw.bg_color Tw.green_200
                                , Tw.mb_5
                                , Tw.transition_all
                                , Tw.duration_500
                                ]
                            ]
                            [ svg
                                [ SvgAttr.height "22px"
                                , SvgAttr.width "22px"
                                , SvgAttr.version "1.1"
                                , SvgAttr.viewBox "0 0 512 512"
                                , SvgAttr.xmlSpace "preserve"
                                , Attr.css
                                    [ Tw.flex_shrink_0
                                    , Tw.inline
                                    , Tw.w_7
                                    , Tw.h_7
                                    , Tw.mr_3
                                    ]
                                , SvgAttr.fill "currentColor"
                                ]
                                [ g []
                                    [ path
                                        [ SvgAttr.d "M474.045,173.813c-4.201,1.371-6.494,5.888-5.123,10.088c7.571,23.199,11.411,47.457,11.411,72.1   c0,62.014-24.149,120.315-68,164.166s-102.153,68-164.167,68s-120.316-24.149-164.167-68S16,318.014,16,256   S40.149,135.684,84,91.833s102.153-68,164.167-68c32.889,0,64.668,6.734,94.455,20.017c28.781,12.834,54.287,31.108,75.81,54.315   c3.004,3.239,8.066,3.431,11.306,0.425c3.24-3.004,3.43-8.065,0.426-11.306c-23-24.799-50.26-44.328-81.024-58.047   C317.287,15.035,283.316,7.833,248.167,7.833c-66.288,0-128.608,25.813-175.48,72.687C25.814,127.392,0,189.712,0,256   c0,66.287,25.814,128.607,72.687,175.479c46.872,46.873,109.192,72.687,175.48,72.687s128.608-25.813,175.48-72.687   c46.873-46.872,72.687-109.192,72.687-175.479c0-26.332-4.105-52.26-12.201-77.064   C482.762,174.736,478.245,172.445,474.045,173.813z"
                                        ]
                                        []
                                    , path
                                        [ SvgAttr.d "M504.969,83.262c-4.532-4.538-10.563-7.037-16.98-7.037s-12.448,2.499-16.978,7.034l-7.161,7.161   c-3.124,3.124-3.124,8.189,0,11.313c3.124,3.123,8.19,3.124,11.314-0.001l7.164-7.164c1.51-1.512,3.52-2.344,5.66-2.344   s4.15,0.832,5.664,2.348c1.514,1.514,2.348,3.524,2.348,5.663s-0.834,4.149-2.348,5.663L217.802,381.75   c-1.51,1.512-3.52,2.344-5.66,2.344s-4.15-0.832-5.664-2.348L98.747,274.015c-1.514-1.514-2.348-3.524-2.348-5.663   c0-2.138,0.834-4.149,2.351-5.667c1.51-1.512,3.52-2.344,5.66-2.344s4.15,0.832,5.664,2.348l96.411,96.411   c1.5,1.5,3.535,2.343,5.657,2.343s4.157-0.843,5.657-2.343l234.849-234.849c3.125-3.125,3.125-8.189,0-11.314   c-3.124-3.123-8.189-3.123-11.313,0L212.142,342.129l-90.75-90.751c-4.533-4.538-10.563-7.037-16.98-7.037   s-12.448,2.499-16.978,7.034c-4.536,4.536-7.034,10.565-7.034,16.977c0,6.412,2.498,12.441,7.034,16.978l107.728,107.728   c4.532,4.538,10.563,7.037,16.98,7.037c6.417,0,12.448-2.499,16.977-7.033l275.847-275.848c4.536-4.536,7.034-10.565,7.034-16.978   S509.502,87.794,504.969,83.262z"
                                        ]
                                        []
                                    ]
                                ]
                            , text info
                            ]
                    )
            )
        ]
