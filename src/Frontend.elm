module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Css
import Css.Global
import Donut
import Html.Styled as Html exposing (Attribute, Html, text)
import Html.Styled.Attributes as Attr
import Html.Styled.Events exposing (onClick, onInput)
import Lamdera exposing (sendToBackend)
import Ports
import Process
import Svgs
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
    , editRoomName = Nothing
    , story = NoStory (Just "Input is empty")
    , error = Nothing
    , storyCount = 1
    , roomId = Nothing
    , stories = []
    , credentials = Admin
    , users = []
    , sessionId = Nothing
    , clock = 0
    , editedStory = NoStory (Just "Input is empty")
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
                            ( { model | status = CreateRoomStep, error = Nothing, name = Nothing, users = { defaultUser | name = validInput, isAdmin = True } :: model.users }
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

        EditRoomName roomName ->
            ( { model | editRoomName = Just roomName }, Cmd.none )

        SendEditedRoom ->
            let
                roomId =
                    model.roomId
                        |> Maybe.withDefault 1
            in
            case validateInput model.roomName of
                Err errorMessage ->
                    ( { model | error = errorMessage }, Cmd.none )

                Ok validInput ->
                    ( { model | editRoomName = Nothing }, sendToBackend <| SignalRoomNameEdit validInput roomId )

        StoreRoom str ->
            ( { model
                | roomName =
                    if str /= "" then
                        Just str

                    else
                        Nothing
                , error = Nothing
              }
            , Cmd.none
            )

        StoreStory str ->
            ( { model
                | story =
                    if str /= "" then
                        Story 0 str
                        --assigning pseudo id

                    else
                        NoStory (Just "Input is empty")
                , error = Nothing
              }
            , Cmd.none
            )

        AddStory ->
            let
                bumpStoryCount =
                    model.storyCount
                        + 1

                storyIdThatIsCurrentlyEdited =
                    case model.editedStory of
                        Story storyId _ ->
                            storyId - 1

                        NoStory _ ->
                            -1
            in
            if model.storyCount == storyIdThatIsCurrentlyEdited then
                ( model
                , Cmd.none
                )

            else
                ( { model
                    | stories = model.stories ++ [ Story bumpStoryCount <| "Story " ++ String.fromInt bumpStoryCount ]
                    , editedStory = Story bumpStoryCount <| "Story " ++ String.fromInt bumpStoryCount
                    , story = Story bumpStoryCount <| "Story " ++ String.fromInt bumpStoryCount
                    , storyCount = bumpStoryCount
                  }
                , Cmd.none
                )

        EditStory storyId storyName ->
            ( { model | editedStory = Story storyId storyName, story = Story storyId storyName }, Cmd.none )

        SendEditedStory targetStoryId ->
            case model.story of
                NoStory _ ->
                    ( { model | story = model.editedStory, editedStory = NoStory (Just "Input is empty") }, Cmd.none )

                Story _ storyName ->
                    let
                        roomId =
                            model.roomId |> Maybe.withDefault 1

                        updatedStories =
                            model.stories
                                |> List.filter
                                    (\story ->
                                        case story of
                                            Story strId _ ->
                                                targetStoryId /= strId

                                            NoStory _ ->
                                                False
                                    )
                                |> List.append [ Story targetStoryId storyName ]
                                |> List.sortBy
                                    (\story ->
                                        case story of
                                            Story strId _ ->
                                                strId

                                            NoStory _ ->
                                                0
                                    )
                    in
                    ( { model | error = Nothing, story = NoStory Nothing, editedStory = NoStory Nothing, stories = updatedStories }
                    , sendToBackend <| SendStoryToBE updatedStories roomId
                    )

        SendStory ->
            case model.story of
                NoStory errorMessage ->
                    ( { model | error = errorMessage }, Cmd.none )

                Story _ storyName ->
                    let
                        bumpStoryCount =
                            model.storyCount + 1
                    in
                    ( { model | error = Nothing, story = NoStory (Just "Input is empty"), stories = model.stories ++ [ Story model.storyCount storyName ], storyCount = bumpStoryCount }, Cmd.none )

        SaveStory ->
            case model.story of
                NoStory errorMessage ->
                    ( { model | story = NoStory errorMessage, error = errorMessage }, Cmd.none )

                Story _ storyName ->
                    let
                        -- TODO think about something smarter
                        roomId =
                            model.roomId |> Maybe.withDefault 1

                        updatedStories =
                            model.stories
                                ++ [ Story model.storyCount storyName ]
                    in
                    ( { model | error = Nothing, status = PokerStep, story = NoStory (Just "Input is empty"), stories = updatedStories }
                    , Cmd.batch [ sendToBackend <| SendStoryToBE updatedStories roomId, Nav.pushUrl model.key <| "/room/" ++ (roomId |> String.fromInt) ]
                    )

        StoreName str ->
            ( { model
                | name =
                    if str /= "" then
                        Just str

                    else
                        Nothing
                , error = Nothing
              }
            , Cmd.none
            )

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

        CopyRoomUrl ->
            ( model, Ports.copyUrlToClipboard <| model.url ++ "invite/" ++ (model.roomId |> Maybe.withDefault 1 |> String.fromInt) )

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

        UpdateRoomName roomName ->
            let
                updatedAnnouncement =
                    model.announcement ++ [ roomName ++ " is new name of the room !" ]
            in
            ( { model | announcement = updatedAnnouncement, roomName = Just roomName }
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
            ( { model | users = users, shouldFlipCards = False }, Cmd.none )

        UsersFlipCards users ->
            ( { model | users = users, shouldFlipCards = not <| model.shouldFlipCards }, Cmd.none )

        UsersCardReset users ->
            ( { model | users = users, shouldFlipCards = False, card = Nothing }, Cmd.none )

        SkipStoryAndExposeCharts users ->
            ( { model | users = users, shouldFlipCards = False, card = Nothing, shouldShowCharts = True, shouldStartClock = False, clock = 0, announcement = [] }, Cmd.none )

        ExposeCharts ->
            ( { model | shouldShowCharts = not <| model.shouldShowCharts, shouldStartClock = False, clock = 0, announcement = [] }, Cmd.none )

        UpdateStories updatedStories ->
            let
                updatedAnnouncement =
                    model.announcement
                        |> (++) [ "Story updated !" ]
            in
            ( { model | stories = updatedStories, announcement = updatedAnnouncement }
            , Process.sleep 4000
                |> Task.perform (\_ -> HideNotification)
            )

        UpdateStoriesAfterSkip updatedStories resetUsers ->
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
                                        |> withSendOnEnter
                                            (Util.onEnterWithCred
                                                Admin
                                                SendName
                                            )
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
                                    [ buttonStyle |> viewButtonWithMsg (SendName Admin) "Save" ]
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
                                        |> withSendOnEnter
                                            (Util.onEnterWithCred
                                                Employee
                                                SendName
                                            )
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
                                    [ buttonStyle |> viewButtonWithMsg (SendName Employee) "Save" ]
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
                                        |> withSendOnEnter
                                            (Util.onEnter
                                                SendRoom
                                            )
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
                                    [ buttonStyle |> viewButtonWithMsg SendRoom "Create" ]
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
                                                        case story of
                                                            Story _ storyName ->
                                                                Html.li
                                                                    [ Attr.css
                                                                        [ Tw.transition_all
                                                                        , Tw.absolute
                                                                        , Tw.w_full
                                                                        ]
                                                                    , Attr.class
                                                                        "hide-after-n"
                                                                    ]
                                                                    [ text storyName ]

                                                            NoStory _ ->
                                                                text ""
                                                    )
                                            )
                                        ]
                                    , Html.input
                                        [ Attr.css <| withError model.error inputStyle
                                        , onInput StoreStory
                                        , Attr.value
                                            (case model.story of
                                                Story _ storyName ->
                                                    storyName

                                                NoStory _ ->
                                                    ""
                                            )
                                        ]
                                        []
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
                                    [ buttonStyle |> viewButtonWithMsg SendStory "Add New"
                                    , buttonStyle |> viewButtonWithMsg SaveStory "Save"
                                    ]
                                ]

                        PokerStep ->
                            Html.div
                                [ Attr.css
                                    [ Tw.px_6
                                    , Tw.flex
                                    , Tw.flex_col
                                    , Tw.text_color Tw.white
                                    , Tw.w_full
                                    , Tw.m_auto
                                    , Bp.lg
                                        [ Tw.flex_row
                                        , Tw.w_5over6
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
                                            [ Tw.pr_10
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
                                        [ Html.div
                                            [ Attr.css
                                                [ case model.editRoomName of
                                                    Just _ ->
                                                        Tw.w_full

                                                    Nothing ->
                                                        Tw.w_auto
                                                ]
                                            ]
                                            [ case model.credentials of
                                                Admin ->
                                                    case model.editRoomName of
                                                        Just _ ->
                                                            Html.div [ Attr.css [ Tw.break_all, Tw.flex, Tw.items_center, Tw.self_start ] ]
                                                                [ inputEditStyle
                                                                    |> withError model.error
                                                                    |> withSendOnEnter
                                                                        (Util.onEnter
                                                                            SendRoom
                                                                        )
                                                                    |> viewInput StoreRoom
                                                                        (model.roomName
                                                                            |> Maybe.withDefault ""
                                                                        )
                                                                , Html.button [ Attr.css buttonEditStyle, onClick SendEditedRoom ] [ text "Save" ]
                                                                ]

                                                        Nothing ->
                                                            Html.h2
                                                                [ Attr.class "edit"
                                                                , Attr.css [ Tw.m_0, Tw.break_all, Tw.flex, Tw.items_center, Tw.cursor_pointer, Tw.pl_2 ]
                                                                , onClick <| EditRoomName (model.roomName |> Maybe.withDefault "Room name not availabe")
                                                                ]
                                                                [ model.roomName |> Maybe.withDefault "Room name is not available" |> text
                                                                , Html.span [ Attr.css [ Tw.flex, Tw.self_stretch, Tw.items_center, Tw.ml_2 ] ] [ Svgs.withOverrideStyle |> Svgs.iconPencil ]
                                                                ]

                                                Employee ->
                                                    Html.h2 [ Attr.css [ Tw.m_0, Tw.break_all, Tw.flex, Tw.items_center ] ] [ model.roomName |> Maybe.withDefault "Room name is not available" |> text ]
                                            ]
                                        , case model.card of
                                            Just _ ->
                                                if model.shouldShowCharts then
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

                                            Nothing ->
                                                text ""
                                        ]
                                    , Html.h4 [ Attr.css [ Tw.text_2xl, Tw.text_color Tw.gray_400, Tw.font_extralight, Tw.break_all ] ]
                                        [ model.stories
                                            |> List.head
                                            |> Maybe.withDefault (Story -1 "There are no more stories")
                                            |> (\story ->
                                                    case story of
                                                        Story _ storyName ->
                                                            text <| "[ Current story ] " ++ storyName

                                                        NoStory _ ->
                                                            text ""
                                               )
                                        ]
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
                                                                [ buttonStyle |> viewButtonWithMsg ResetTime "Reset timer"
                                                                , if model.shouldFlipCards then
                                                                    buttonStyle |> viewButtonWithMsg HideCards "Hide Votes "

                                                                  else
                                                                    buttonStyle |> viewButtonWithMsg ShowCards "Show votes"
                                                                , buttonStyle |> viewButtonWithMsg ClearVotes "Clear votes"
                                                                , buttonStyle |> viewButtonWithMsg SkipStory "Skip story"
                                                                , if model.users |> List.all (\user -> not <| user.voteState == NotVoted) then
                                                                    buttonStyle |> viewButtonWithMsg FinishVoting "Finish Voting"

                                                                  else
                                                                    text ""
                                                                ]

                                                        else
                                                            buttonStyle |> viewButtonWithMsg StartTime "Start timer"

                                                      else
                                                        text ""
                                                    ]

                                            Employee ->
                                                text ""
                                        ]
                                    , Html.div [ Attr.css [ Tw.mt_10, Tw.flex, Tw.gap_4, Tw.items_start ] ]
                                        [ Html.h4 [ Attr.css [ Tw.text_3xl, Tw.m_0, Tw.mb_4 ] ] [ text "Stories:" ]
                                        , case model.credentials of
                                            Admin ->
                                                buttonStyle |> viewButtonWithMsg AddStory "+ 1"

                                            Employee ->
                                                text ""
                                        ]
                                    , Html.ul [ Attr.css [ Tw.list_none, Tw.flex, Tw.p_0, Tw.m_0, Tw.flex_col, Tw.text_2xl, Tw.gap_2 ] ]
                                        (model.stories
                                            |> List.map
                                                (\story ->
                                                    case story of
                                                        Story storyId storyName ->
                                                            case model.credentials of
                                                                Admin ->
                                                                    Html.li
                                                                        [ Attr.css
                                                                            [ Tw.break_all
                                                                            , Tw.flex
                                                                            , Tw.items_center
                                                                            , Tw.self_start
                                                                            , if model.editedStory == Story storyId storyName then
                                                                                Tw.w_full

                                                                              else
                                                                                Tw.w_auto
                                                                            ]
                                                                        ]
                                                                        [ if model.editedStory == Story storyId storyName then
                                                                            Html.div [ Attr.css [ Tw.break_all, Tw.flex, Tw.items_center, Tw.self_start, Tw.w_full ] ]
                                                                                [ Html.input
                                                                                    [ Attr.css <| withError model.error inputEditStyle
                                                                                    , onInput StoreStory
                                                                                    , Attr.value
                                                                                        (case model.story of
                                                                                            Story _ sn ->
                                                                                                sn

                                                                                            NoStory _ ->
                                                                                                ""
                                                                                        )
                                                                                    ]
                                                                                    []
                                                                                , Html.button [ Attr.css buttonEditStyle, onClick (SendEditedStory storyId) ] [ text "Save" ]
                                                                                ]

                                                                          else
                                                                            Html.div [ Attr.class "edit", Attr.css [ Tw.break_all, Tw.flex, Tw.items_center, Tw.cursor_pointer, Tw.self_start, Tw.pl_2, Tw.w_full, Tw.h_full ], onClick <| EditStory storyId storyName ]
                                                                                [ text storyName
                                                                                , Html.span [ Attr.css [ Tw.flex, Tw.self_stretch, Tw.ml_2, Tw.items_center, Tw.py_1 ] ] [ Svgs.withOverrideStyle |> Svgs.iconPencil ]
                                                                                ]
                                                                        ]

                                                                Employee ->
                                                                    Html.li [ Attr.css [ Tw.break_all, Tw.py_2 ] ] [ text storyName ]

                                                        NoStory _ ->
                                                            text ""
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
                                                    , Tw.rounded_b_none
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
                                                        , Tw.ring_color Tw.slate_900
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
                                        , buttonStyle |> withReadmeInput |> viewButtonWithMsg CopyRoomUrl "Copy URL"
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
                Html.span [ Attr.class "checkmark" ] []

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
                ]
            ]
            (Util.cards
                |> List.map
                    (\card ->
                        Html.li
                            [ case model.card of
                                Just crd ->
                                    if crd == card.name then
                                        Attr.class "selected"

                                    else
                                        Attr.css []

                                Nothing ->
                                    Attr.css []
                            , Attr.css
                                [ Tw.flex
                                , Tw.border_color Tw.white
                                , Tw.border_solid
                                , Tw.border_2
                                , Tw.justify_center
                                , Tw.w_1over3
                                , Tw.items_center
                                , Tw.h_28
                                , Tw.transition_all
                                , Tw.duration_300
                                , Tw.p_0
                                , Tw.overflow_hidden
                                , Tw.relative
                                , Tw.cursor_pointer
                                , Bp.md
                                    [ Tw.w_1over4, Tw.h_44 ]
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
                                            , Tw.font_bold
                                            , Tw.text_4xl
                                            , Tw.p_0
                                            , Css.hover
                                                [ Tw.p_0
                                                ]
                                            , Bp.lg
                                                [ Tw.text_7xl
                                                ]
                                            ]

                                    else
                                        Attr.css
                                            [ Tw.p_5
                                            , Css.hover
                                                [ Tw.bg_color Tw.white
                                                , Tw.border_color Tw.white
                                                , Tw.text_color Tw.black
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
                                                [ Html.div [ Attr.css [ Tw.flex, Tw.flex_col, Tw.gap_4, Bp.lg [ Tw.flex_row ] ] ]
                                                    [ Html.div [ Attr.css [ Tw.flex, Tw.gap_4 ] ]
                                                        [ Html.span [ Attr.css [ Css.width (Css.px 31), Css.height (Css.px 31), Tw.bg_color (Util.getColor int), Tw.hidden, Bp.lg [ Tw.block ] ] ] []
                                                        , Html.span [ Attr.css [ Css.minWidth (Css.px 40) ] ] [ entry.uniqueVoteValue |> Maybe.withDefault 0 |> String.fromFloat |> text ]
                                                        , Html.span [ Attr.css [ Css.minWidth (Css.px 57) ] ] [ text <| Util.roundFloat entry.percentage 2 ++ "%" ]
                                                        , Html.span [ Attr.css [ Css.minWidth (Css.px 123) ] ] [ text <| "(" ++ (entry.numOfVoters |> String.fromFloat) ++ " " ++ pluralification entry.numOfVoters "player" ++ ")" ]
                                                        ]
                                                    , Html.div [ Attr.css [ Tw.w_full, Tw.bg_color Tw.slate_900, Tw.h_8 ] ]
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
                        Html.div [ Attr.css [ Tw.flex, Tw.flex_1, Tw.flex_col, Bp.lg [ Tw.flex_row, Tw.h_96 ] ] ]
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
            buttonStyle |> viewButtonWithMsg NextStory "Next Story"

          else
            text "No more stories, add another one ? "
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


buttonEditStyle : List Css.Style
buttonEditStyle =
    [ Tw.bg_color Tw.teal_400
    , Tw.text_color Tw.white
    , Tw.py_2
    , Tw.px_2
    , Tw.text_xl
    , Tw.border
    , Tw.border_color Tw.teal_400
    , Tw.rounded_sm
    , Tw.rounded_l_none
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
    , Tw.text_lg
    , Tw.ring_color Tw.gray_300
    , Css.focus
        [ Tw.ring_2
        , Tw.ring_inset
        , Tw.ring_color Tw.teal_400
        ]
    ]


inputEditStyle : List Css.Style
inputEditStyle =
    [ Tw.block
    , Tw.form_input
    , Tw.rounded_sm
    , Tw.rounded_r_none
    , Tw.border_0
    , Tw.py_2
    , Tw.px_2
    , Tw.shadow_sm
    , Tw.ring_1
    , Tw.h_12
    , Tw.text_2xl
    , Tw.flex_1
    , Tw.font_light
    , Tw.ring_inset
    , Tw.ring_color Tw.gray_300
    , Tw.bg_color Tw.slate_900
    , Tw.text_color Tw.white
    , Css.focus
        [ Tw.outline_0
        , Tw.ring_0
        ]
    ]


withReadmeInput : List Css.Style -> List Css.Style
withReadmeInput basicStyle =
    basicStyle ++ [ Tw.rounded_t_none ]


withError : InvalidTextFiled -> List Css.Style -> List Css.Style
withError maybeError basicStyle =
    case maybeError of
        Just _ ->
            basicStyle
                ++ [ Tw.border_2
                   , Tw.border_color Tw.red_500
                   , Tw.border_solid
                   , Css.focus
                        [ Tw.border_0
                        ]
                   ]

        Nothing ->
            basicStyle


withSendOnEnter : Attribute FrontendMsg -> List Css.Style -> List (Attribute FrontendMsg)
withSendOnEnter attr styles =
    [ attr, Attr.css styles ]


viewInput : (String -> FrontendMsg) -> String -> List (Attribute FrontendMsg) -> Html FrontendMsg
viewInput toMsg value attrs =
    Html.input
        ([ Attr.type_ "text"
         , onInput toMsg
         , Attr.value value
         ]
            ++ attrs
        )
        []


viewButtonWithMsg : FrontendMsg -> String -> List Css.Style -> Html FrontendMsg
viewButtonWithMsg msg label styles =
    Html.button [ Attr.css styles, onClick msg ] [ text label ]


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
            [ Svgs.iconError
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
                                , Tw.text_center
                                , Tw.text_color Tw.green_800
                                , Tw.bg_color Tw.green_200
                                , Tw.mb_5
                                , Tw.transition_all
                                , Tw.duration_500
                                ]
                            ]
                            [ Svgs.iconCheck
                            , text info
                            ]
                    )
            )
        ]
