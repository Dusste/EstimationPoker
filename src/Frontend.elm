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
import Svg.Styled exposing (circle, g, path, svg, tspan)
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
    , chart = Bar
    , shouldStartClock = False
    , shouldFlipCards = False
    , shouldShowCharts = False
    , shouldStartChartAnimation = False
    , card = Nothing
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
                    ( { model | users = updatedUsers, card = Just cardName }, sendToBackend <| SendCard cardValue (model.roomId |> Maybe.withDefault 1) )

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
            ( { model | users = users, shouldFlipCards = False, card = Nothing }, Cmd.none )

        SkipStoryAndExposeCharts users ->
            ( { model | users = users, shouldFlipCards = False, card = Nothing, shouldShowCharts = True, shouldStartClock = False, clock = 0 }, Cmd.none )

        ExposeCharts ->
            ( { model | shouldShowCharts = not <| model.shouldShowCharts, shouldStartClock = False, clock = 0 }, Cmd.none )

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
                    [ Html.div
                        [ Attr.css
                            [ Tw.p_4
                            , Tw.absolute
                            , Tw.w_full
                            , Tw.transition_opacity
                            , Tw.flex
                            , Tw.duration_200
                            , Tw.ease_in
                            , case model.error of
                                Just _ ->
                                    Tw.opacity_100

                                Nothing ->
                                    Tw.opacity_0
                            , Tw.mb_4
                            , Tw.text_lg
                            , Tw.justify_center
                            , Tw.text_color Tw.red_800
                            , Tw.bg_color Tw.red_200
                            ]
                        , Attr.attribute "role" "alert"
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
                        , case model.error of
                            Just err ->
                                text err

                            Nothing ->
                                text ""
                        ]
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

                                -- , svg [] [ PieChart.main ]
                                , Html.div
                                    [ Attr.css
                                        [ Bp.sm
                                            [ Tw.mx_auto
                                            , Tw.w_full
                                            , Tw.max_w_sm
                                            ]
                                        ]
                                    ]
                                    [ Html.p [ Attr.css [ Tw.text_color Tw.gray_400, Tw.text_lg, Tw.italic, Tw.mb_4, Tw.mt_0 ] ] [ text "[ You're about to become an admin ]" ]
                                    , viewInput StoreName
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
                                    [ Html.h2 [] [ model.roomName |> Maybe.withDefault "{default room name}" |> text ] ]
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
                                    , Html.p [ Attr.css [ Tw.text_color Tw.gray_400, Tw.text_lg, Tw.italic, Tw.mb_4, Tw.mt_0 ] ] [ text "[ After that we will redirect you to team's room ]" ]
                                    , viewInput StoreName
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
                                    [ Html.p [ Attr.css [ Tw.text_color Tw.gray_400, Tw.text_lg, Tw.italic, Tw.mb_4, Tw.mt_0 ] ] [ text "[ Place where you can vote for stories ]" ]
                                    , viewInput StoreRoom
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
                                    [ Html.p [ Attr.css [ Tw.text_color Tw.gray_400, Tw.text_lg, Tw.italic, Tw.mb_4, Tw.mt_0 ] ] [ text "[ Add multiple or one story ]" ]
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
                                    , viewInput StoreStory
                                        (model.story
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
                                            , Tw.flex
                                            , Tw.gap_6
                                            , Tw.justify_center
                                            ]
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
                                    , Tw.flex_row
                                    , Tw.text_color Tw.white
                                    , Tw.max_w_7xl
                                    , Tw.m_auto
                                    ]
                                ]
                                [ Html.div
                                    [ Attr.css
                                        [ Tw.flex
                                        , Tw.flex_col
                                        , Tw.flex_1
                                        , Tw.pr_4
                                        , Tw.py_6
                                        ]
                                    ]
                                    [ Html.div
                                        [ Attr.css
                                            [ Tw.text_color Tw.white
                                            , Tw.text_2xl
                                            , Tw.flex
                                            , Tw.flex_row
                                            , Tw.justify_between
                                            , Tw.items_center
                                            , Bp.sm
                                                [ Tw.gap_10 ]
                                            ]
                                        ]
                                        [ Html.h2 [ Attr.css [ Tw.m_0 ] ] [ model.roomName |> Maybe.withDefault "Room name is not available" |> text ]
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
                                    , Html.h4 [ Attr.css [ Tw.text_2xl, Tw.text_color Tw.gray_400 ] ] [ model.stories |> List.head |> Maybe.withDefault "There are no more stories" |> (++) "[ Current story ] " |> text ]
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
                                                Html.div []
                                                    [ if not <| model.shouldShowCharts && List.length model.stories > 0 then
                                                        if model.shouldStartClock then
                                                            Html.div [ Attr.css [ Tw.flex, Tw.gap_4 ] ]
                                                                [ viewButtonWithMsg ResetTime "Reset timer"
                                                                , viewButtonWithMsg FlipCards "Flip cards"
                                                                , viewButtonWithMsg ClearVotes "Clear votes"
                                                                , viewButtonWithMsg SkipStory "Skip story"
                                                                , if model.users |> List.all (\user -> user.hasVoted) then
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
                                                    Html.li [] [ text story ]
                                                )
                                        )
                                    ]
                                , Html.div
                                    [ Attr.css
                                        [ Tw.text_right
                                        , Tw.border_l
                                        , Tw.border_color Tw.gray_600
                                        , Tw.border_solid
                                        , Tw.border_r_0
                                        , Tw.border_b_0
                                        , Tw.border_t_0
                                        , Tw.pl_10
                                        , Tw.flex
                                        , Tw.items_center
                                        , Tw.py_6
                                        ]
                                    ]
                                    [ Html.div [ Attr.css [ Tw.flex, Tw.flex_col ] ]
                                        [ Html.div [ Attr.css [ Tw.text_5xl ] ] [ text <| Util.fromIntToCounter model.clock ]
                                        , Html.p [ Attr.css [ Tw.text_2xl, Tw.text_color Tw.gray_400 ] ] [ text "[ Copy link and send to collegues ]" ]
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
                                                    , Tw.text_right
                                                    , Css.focus
                                                        [ Tw.outline_0
                                                        , Tw.ring_color Tw.gray_300
                                                        ]
                                                    , Bp.sm
                                                        [ Tw.text_lg
                                                        , Tw.leading_6
                                                        ]
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
                                                        (\{ isAdmin, name, card, hasVoted } ->
                                                            if isAdmin then
                                                                Html.li [ Attr.css [ Tw.flex, Tw.justify_end, Tw.gap_4, Tw.text_color Tw.blue_400 ] ]
                                                                    [ Html.div []
                                                                        [ case model.credentials of
                                                                            Admin ->
                                                                                case card of
                                                                                    Just crd ->
                                                                                        Html.span [ Attr.css [] ] [ crd |> String.fromFloat |> text ]

                                                                                    Nothing ->
                                                                                        text ""

                                                                            Employee ->
                                                                                if model.shouldFlipCards then
                                                                                    case card of
                                                                                        Just crd ->
                                                                                            Html.span [ Attr.css [] ] [ crd |> String.fromFloat |> text ]

                                                                                        Nothing ->
                                                                                            text ""

                                                                                else
                                                                                    text ""
                                                                        ]
                                                                    , Html.p [ Attr.css [ Tw.m_0 ] ] [ text name ]
                                                                    ]

                                                            else
                                                                case model.credentials of
                                                                    Admin ->
                                                                        Html.li [ Attr.css [ Tw.flex, Tw.justify_end, Tw.gap_4 ] ]
                                                                            [ Html.div []
                                                                                [ case card of
                                                                                    Just crd ->
                                                                                        Html.span [ Attr.css [] ] [ crd |> String.fromFloat |> text ]

                                                                                    Nothing ->
                                                                                        text ""
                                                                                ]
                                                                            , Html.p [ Attr.css [ Tw.m_0 ] ] [ text name ]
                                                                            ]

                                                                    Employee ->
                                                                        Html.li [ Attr.css [ Tw.flex, Tw.justify_end, Tw.gap_4 ] ]
                                                                            [ Html.div []
                                                                                [ if hasVoted && not model.shouldFlipCards then
                                                                                    Html.span [ Attr.css [ Tw.m_0 ] ] [ 0xA936 |> Char.fromCode |> String.fromChar |> text ]

                                                                                  else
                                                                                    text ""
                                                                                ]
                                                                            , if model.shouldFlipCards then
                                                                                case card of
                                                                                    Just crd ->
                                                                                        Html.p [ Attr.css [ Tw.m_0 ] ]
                                                                                            [ Html.span [] [ crd |> String.fromFloat |> text ]
                                                                                            ]

                                                                                    Nothing ->
                                                                                        text ""

                                                                              else
                                                                                text ""
                                                                            , Html.p [ Attr.css [ Tw.m_0 ] ] [ text name ]
                                                                            ]
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


viewCards : FrontendModel -> Html FrontendMsg
viewCards model =
    Html.div []
        [ Html.h3 [ Attr.css [ Tw.text_color Tw.gray_400, Tw.font_light ] ] [ text "[ Pick card to estimate story ]" ]
        , Html.ul [ Attr.css [ Tw.list_none, Tw.flex, Tw.flex_wrap, Tw.p_0, Tw.m_0, Tw.gap_10, Tw.text_2xl ] ]
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
                                , Tw.w_1over4
                                , Tw.transition_all
                                , Tw.duration_300
                                , Tw.relative
                                , Tw.cursor_pointer
                                ]
                            , if model.shouldStartClock then
                                onClick (ChooseCard card.value card.name)

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
                                                [ Html.div [ Attr.css [ Tw.flex, Tw.gap_4, Tw.justify_end ] ]
                                                    [ Html.div [ Attr.css [ Tw.flex, Tw.gap_4 ] ]
                                                        [ Html.span [ Attr.css [ Css.width (Css.px 31), Css.height (Css.px 31), Tw.bg_color (Util.getColor int) ] ] []
                                                        , Html.span [ Attr.css [ Css.minWidth (Css.px 40) ] ] [ entry.uniqueVoteValue |> Maybe.withDefault 0 |> String.fromFloat |> text ]
                                                        , Html.span [ Attr.css [ Css.minWidth (Css.px 57) ] ] [ text <| (entry.percentage |> String.fromFloat) ++ "%" ]
                                                        , Html.span [ Attr.css [ Css.minWidth (Css.px 123) ] ] [ text <| "(" ++ (entry.numOfVoters |> String.fromFloat) ++ " " ++ pluralification entry.numOfVoters "player" ++ ")" ]
                                                        ]
                                                    , Html.div [ Attr.css [ Tw.w_96, Tw.bg_color Tw.slate_900, Tw.h_8 ] ]
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
                        Html.div [ Attr.css [ Tw.flex, Tw.flex_1 ] ]
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
                                                    [ Html.div [ Attr.css [ Tw.flex, Tw.gap_4, Tw.justify_end ] ]
                                                        [ Html.div [ Attr.css [ Tw.flex, Tw.gap_4 ] ]
                                                            [ Html.span [ Attr.css [ Css.width (Css.px 31), Css.height (Css.px 31), Tw.bg_color (Util.getColor int) ] ] []
                                                            , Html.span [ Attr.css [ Css.minWidth (Css.px 40) ] ] [ entry.uniqueVoteValue |> Maybe.withDefault 0 |> String.fromFloat |> text ]
                                                            , Html.span [ Attr.css [ Css.minWidth (Css.px 57) ] ] [ text <| (entry.percentage |> String.fromFloat) ++ "%" ]
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
        [ Tw.bg_color Tw.white
        , Tw.text_color Tw.black
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


viewButtonWithMsg : FrontendMsg -> String -> Html FrontendMsg
viewButtonWithMsg msg label =
    Html.button [ Attr.css buttonStyle, onClick msg ] [ text label ]


viewInput : (String -> FrontendMsg) -> String -> Html FrontendMsg
viewInput toMsg value =
    Html.input
        [ Attr.type_ "text"
        , Attr.css inputStyle
        , onInput toMsg
        , Attr.value value
        ]
        []
