module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Button
import Css
import Css.Global
import Donut
import Html.Styled as Html exposing (Attribute, Html, text)
import Html.Styled.Attributes as Attr
import Html.Styled.Events exposing (onClick, onInput)
import Input
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
    , status = Intro
    , name = ""
    , roomName = ""
    , sequenceAsInput = ""
    , editedRoomName = WrappedEditState
    , story = NoStory
    , error = Nothing
    , storyCount = 1
    , roomId = Nothing
    , stories = []
    , credentials = Admin
    , users = []
    , shouldEnableCustomSequence = False
    , sequence = Reject
    , chooseSequence = Default
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

        StartAsAdmin ->
            ( { model | status = EnterAdminNameStep }, Cmd.none )

        SendName cred ->
            case Util.validateInput model.name of
                Err errorMessage ->
                    ( { model | error = Just errorMessage }, Cmd.none )

                Ok validInput ->
                    case cred of
                        Admin ->
                            ( { model
                                | status = CreateRoomStep
                                , error = Nothing
                                , name = Util.fromValidTextFieldToString validInput
                                , users =
                                    { card = Nothing
                                    , sessionId = "Invalid session id"
                                    , voteState = NotVoted
                                    , name = validInput
                                    , isAdmin = True
                                    }
                                        :: model.users
                              }
                            , sendToBackend <| SendAdminNameToBE validInput
                            )

                        Employee ->
                            case model.sessionId of
                                Just sessionId ->
                                    ( { model | status = PokerStep, error = Nothing, name = "", users = { card = Nothing, voteState = NotVoted, isAdmin = False, sessionId = sessionId, name = validInput } :: model.users }
                                    , Cmd.batch
                                        [ sendToBackend <| SendUserNameToBE validInput (model.roomId |> Maybe.withDefault 1)
                                        , Nav.pushUrl model.key <| "/room/" ++ (model.roomId |> Maybe.withDefault 1 |> String.fromInt)
                                        ]
                                    )

                                Nothing ->
                                    ( model, Cmd.none )

        SendRoom ->
            case Util.validateInput model.roomName of
                Err errorMessage ->
                    ( { model | error = Just errorMessage }, Cmd.none )

                Ok validInput ->
                    let
                        roomId =
                            model.roomId
                                |> Maybe.withDefault 1
                    in
                    ( { model | status = CreateStoryStep, error = Nothing }
                    , sendToBackend <| SendRoomNameToBE validInput roomId
                    )

        EditRoomName ->
            ( { model | editedRoomName = UnwrappedEditState }, Cmd.none )

        SendEditedRoom ->
            let
                roomId =
                    model.roomId
                        |> Maybe.withDefault 1
            in
            case Util.validateInput model.roomName of
                Err errorMessage ->
                    ( { model | error = Just errorMessage }, Cmd.none )

                Ok validInput ->
                    ( { model | editedRoomName = WrappedEditState }, sendToBackend <| SignalRoomNameEdit validInput roomId )

        StoreRoom str ->
            ( { model | roomName = str, error = Nothing }, Cmd.none )

        StoreEditStory str ->
            ( { model
                | story =
                    if str /= "" then
                        Edit 0 <| ValidTextField str
                        --assigning pseudo id

                    else
                        NoStory
                , error = Nothing
              }
            , Cmd.none
            )

        StoreStory str ->
            ( { model
                | story =
                    if str /= "" then
                        Story 0 <| ValidTextField str
                        --assigning pseudo id

                    else
                        NoStory
                , error = Nothing
              }
            , Cmd.none
            )

        AddStory ->
            let
                bumpStoryCount =
                    model.storyCount
                        + 1

                -- When adding new story we're wrapping up all previous stories
                toWrappedStories =
                    model.stories
                        |> List.map
                            (\str ->
                                case str of
                                    Edit strId strName ->
                                        Story strId strName

                                    _ ->
                                        str
                            )
            in
            ( { model
                | stories = toWrappedStories ++ [ Edit bumpStoryCount <| ValidTextField <| "Story " ++ String.fromInt bumpStoryCount ]
                , story = Edit bumpStoryCount <| ValidTextField <| "Story " ++ String.fromInt bumpStoryCount
                , storyCount = bumpStoryCount
              }
            , Cmd.none
            )

        EditStory editStoryId editStoryName ->
            let
                updatedStories =
                    model.stories
                        |> List.map
                            (\str ->
                                case str of
                                    Story id strName ->
                                        if id == editStoryId then
                                            Edit editStoryId strName

                                        else
                                            str

                                    Edit strId strName ->
                                        Story strId strName

                                    NoStory ->
                                        str
                            )
            in
            ( { model | story = Edit editStoryId editStoryName, stories = updatedStories }, Cmd.none )

        SendEditedStory targetStoryId ->
            case model.story of
                Edit _ storyName ->
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

                                            _ ->
                                                False
                                    )
                                |> List.append [ Story targetStoryId storyName ]
                                |> List.sortBy
                                    (\story ->
                                        case story of
                                            Story strId _ ->
                                                strId

                                            Edit _ _ ->
                                                0

                                            NoStory ->
                                                0
                                    )
                    in
                    ( { model | error = Nothing, story = NoStory, stories = updatedStories }
                    , sendToBackend <| SendStoryToBE updatedStories roomId
                    )

                _ ->
                    ( model, Cmd.none )

        SendStory ->
            case model.story of
                Story _ storyName ->
                    let
                        bumpStoryCount =
                            model.storyCount + 1
                    in
                    ( { model | error = Nothing, story = NoStory, stories = model.stories ++ [ Story model.storyCount storyName ], storyCount = bumpStoryCount }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SaveStory ->
            case model.story of
                Story _ storyName ->
                    let
                        -- TODO think about something smarter
                        roomId =
                            model.roomId |> Maybe.withDefault 1

                        updatedStories =
                            model.stories
                                ++ [ Story model.storyCount storyName ]
                    in
                    ( { model | error = Nothing, status = StoryPointsSequenceStep, story = NoStory, stories = updatedStories }
                    , Cmd.batch [ sendToBackend <| SendStoryToBE updatedStories roomId ]
                    )

                _ ->
                    ( model, Cmd.none )

        StoreName str ->
            ( { model | name = str, error = Nothing }, Cmd.none )

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
                    ( { model | users = updatedUsers, card = Just cardName }, sendToBackend <| SendCard cardValue (model.roomId |> Maybe.withDefault 1) justSessionId )

                Nothing ->
                    ( model, Cmd.none )

        Tick _ ->
            ( { model | clock = model.clock + 1 }, Cmd.none )

        StoreSequence str ->
            ( { model
                | sequenceAsInput = str
                , sequence =
                    if str == "" then
                        Reject

                    else
                        model.sequence
                , error = Nothing
              }
            , Cmd.none
            )

        CheckSequence ->
            ( { model | sequence = Util.fromStringToSequence model.sequenceAsInput, error = Nothing }, Cmd.none )

        SendSequence ->
            ( { model | status = PokerStep }
            , Cmd.batch
                [ sendToBackend <| SendSequenceToBE model.chooseSequence (model.roomId |> Maybe.withDefault 1)
                , Nav.pushUrl model.key <| "/room/" ++ (model.roomId |> Maybe.withDefault 1 |> String.fromInt)
                ]
            )

        SendCustomSequence ->
            case model.sequence of
                Accept sequence ->
                    ( { model | status = PokerStep }
                    , Cmd.batch
                        [ sendToBackend <|
                            SendCustomSequenceToBE sequence
                                (model.roomId |> Maybe.withDefault 1)
                        , Nav.pushUrl model.key <| "/room/" ++ (model.roomId |> Maybe.withDefault 1 |> String.fromInt)
                        ]
                    )

                InTransition _ ->
                    ( model, Cmd.none )

                Reject ->
                    ( model, Cmd.none )

        SelectSequence option ->
            ( { model | chooseSequence = option }, Cmd.none )

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
            let
                updatedAnnouncement =
                    List.append model.announcement [ " URL copied !" ]
            in
            ( { model | announcement = updatedAnnouncement }
            , Cmd.batch
                [ Ports.copyUrlToClipboard <| model.url ++ "invite/" ++ (model.roomId |> Maybe.withDefault 1 |> String.fromInt)
                , Process.sleep 4000
                    |> Task.perform (\_ -> HideNotification)
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

        ShowChart chart ->
            ( { model | chart = chart }, Cmd.none )

        HideNotification ->
            ( { model | announcement = List.drop 1 model.announcement }, Cmd.none )

        EnableSequenceInput ->
            ( { model | shouldEnableCustomSequence = not <| model.shouldEnableCustomSequence }, Cmd.none )

        NoOpWithText _ ->
            ( model, Cmd.none )


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
                , roomName = Util.fromValidTextFieldToString roomName
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
                    List.append model.announcement [ Util.fromValidTextFieldToString name ++ " has joined !" ]
            in
            ( { model | users = { voteState = NotVoted, card = Nothing, sessionId = sessionId, name = name, isAdmin = False } :: model.users, announcement = updatedAnnouncement }
            , Process.sleep 4000
                |> Task.perform (\_ -> HideNotification)
            )

        UpdateRoomName roomName ->
            let
                updatedAnnouncement =
                    List.append model.announcement [ Util.fromValidTextFieldToString roomName ++ " is new name of the room !" ]
            in
            ( { model | announcement = updatedAnnouncement, roomName = Util.fromValidTextFieldToString roomName }
            , Process.sleep 4000
                |> Task.perform (\_ -> HideNotification)
            )

        SupplyBEData { stories, users } ->
            ( { model | stories = stories, users = users }, Cmd.none )

        UsersStartTimer ->
            let
                updatedAnnouncement =
                    List.append model.announcement [ "Start voting !" ]
            in
            ( { model | shouldStartClock = True, announcement = updatedAnnouncement }
            , Process.sleep 4000
                |> Task.perform (\_ -> HideNotification)
            )

        UsersResetTimer ->
            ( { model | shouldStartClock = False, clock = 0, announcement = [] }, Cmd.none )

        UpdateCards users userWhoVoted ->
            let
                user =
                    model.users
                        |> List.filter (\u -> u.sessionId == userWhoVoted)
                        |> List.head
                        |> Maybe.withDefault defaultUser

                updatedAnnouncement =
                    List.append model.announcement [ Util.fromValidTextFieldToString user.name ++ " has voted !" ]
            in
            ( { model | users = users, shouldFlipCards = False, announcement = updatedAnnouncement }
            , Process.sleep 4000
                |> Task.perform (\_ -> HideNotification)
            )

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
                    List.append model.announcement [ "Story updated !" ]
            in
            ( { model | stories = updatedStories, announcement = updatedAnnouncement }
            , Process.sleep 4000
                |> Task.perform (\_ -> HideNotification)
            )

        UpdateStoriesAfterSkip updatedStories resetUsers ->
            ( { model | stories = updatedStories, shouldShowCharts = not <| model.shouldShowCharts, users = resetUsers, shouldStartChartAnimation = False }, Cmd.none )

        ChartAnimation ->
            ( { model | shouldStartChartAnimation = True }, Cmd.none )

        UpdateSequence seq ->
            ( { model | chooseSequence = seq }, Cmd.none )


view : Model -> Browser.Document FrontendMsg
view model =
    { title = "EstPoker"
    , body =
        [ Html.toUnstyled <|
            Html.div [ Attr.css [ Tw.flex, Tw.h_full, Tw.bg_color Tw.black, Tw.h_screen ] ]
                [ Html.div [ Attr.css [ Tw.h_full, Tw.w_full, Tw.flex, Tw.flex_col ] ]
                    [ viewNotifications model
                    , case model.status of
                        Intro ->
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
                                            , Tw.max_w_2xl
                                            ]
                                        ]
                                    ]
                                    [ Html.h1 [ Attr.css [ Tw.mb_20, Tw.leading_relaxed, Bp.lg [ Tw.leading_none ] ], Attr.attribute "data-testid" "intro-h1" ] [ text "Hi ! Welcome to EST Poker !" ]
                                    , Html.p [ Attr.css [ Tw.text_color Tw.gray_400, Tw.text_2xl, Tw.italic, Tw.mb_10, Tw.mt_0, Tw.font_extralight ], Attr.attribute "data-testid" "intro-sub-head" ] [ text "[ Simple web app for estimation story points within team ]" ]
                                    , Html.p [ Attr.css [ Tw.rounded, Tw.border_color Tw.teal_400, Tw.border_2, Tw.border_solid, Tw.p_5, Tw.mb_10 ], Attr.attribute "data-testid" "intro-punchline" ] [ text "Precise Planning, Efficient Execution, Blazing Fast !" ]
                                    , Button.new
                                        |> Button.withText "Get Started →"
                                        |> Button.withJumboStyle
                                        |> Button.withOnClick StartAsAdmin
                                        |> Button.withTestId "intro-submit"
                                        |> Button.toHtml
                                    ]
                                ]

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
                                    [ Html.h2 [ Attr.css [ Tw.mt_0, Tw.mb_4 ], Attr.attribute "data-testid" "enter-name-admin-text" ] [ text "Add your name" ] ]
                                , Html.div
                                    [ Attr.css
                                        [ Bp.sm
                                            [ Tw.mx_auto
                                            , Tw.w_full
                                            , Tw.max_w_sm
                                            ]
                                        ]
                                    ]
                                    [ Html.p [ Attr.css [ Tw.text_color Tw.gray_400, Tw.text_lg, Tw.italic, Tw.mb_4, Tw.mt_0, Tw.font_extralight ], Attr.attribute "data-testid" "enter-name-admin-info-text" ]
                                        [ text "[ You're about to become an admin ]" ]
                                    , Input.new
                                        |> Input.withHandleInput
                                            StoreName
                                            model.name
                                        |> Input.withSendOnEnter (SendName Admin)
                                        |> Input.withPlaceholder "eq: Steve"
                                        |> Input.withPrimaryStyles
                                        |> Input.withError model.error
                                        |> Input.withTestId "enter-name-admin-input"
                                        |> Input.toHtml
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
                                    [ Button.new
                                        |> Button.withText "Save"
                                        |> Button.withPrimaryStyle
                                        |> Button.withOnClick (SendName Admin)
                                        |> Button.withTestId "enter-name-admin-submit"
                                        |> Button.toHtml
                                    ]
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
                                    , Html.h2 [] [ model.roomName |> text ]
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
                                    , Input.new
                                        |> Input.withHandleInput
                                            StoreName
                                            model.name
                                        |> Input.withSendOnEnter (SendName Employee)
                                        |> Input.withPlaceholder "eq: Mark"
                                        |> Input.withPrimaryStyles
                                        |> Input.withError model.error
                                        |> Input.toHtml
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
                                    [ Button.new
                                        |> Button.withText "Save"
                                        |> Button.withPrimaryStyle
                                        |> Button.withOnClick (SendName Employee)
                                        |> Button.toHtml
                                    ]
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
                                    [ Html.h2 [ Attr.css [ Tw.mt_0, Tw.mb_4 ], Attr.attribute "data-testid" "enter-room-text" ] [ text "Create new room" ] ]
                                , Html.div
                                    [ Attr.css
                                        [ Bp.sm
                                            [ Tw.mx_auto
                                            , Tw.w_full
                                            , Tw.max_w_sm
                                            ]
                                        ]
                                    ]
                                    [ Html.p [ Attr.css [ Tw.text_color Tw.gray_400, Tw.text_lg, Tw.italic, Tw.mb_4, Tw.mt_0, Tw.font_extralight ], Attr.attribute "data-testid" "enter-room-info-text" ] [ text "[ Place where you can vote for stories ]" ]
                                    , Input.new
                                        |> Input.withHandleInput
                                            StoreRoom
                                            model.roomName
                                        |> Input.withSendOnEnter SendRoom
                                        |> Input.withPlaceholder "eq: Engineering ninjas !"
                                        |> Input.withPrimaryStyles
                                        |> Input.withError model.error
                                        |> Input.withTestId "enter-room-input"
                                        |> Input.toHtml
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
                                    [ Button.new
                                        |> Button.withText "Create"
                                        |> Button.withPrimaryStyle
                                        |> Button.withOnClick SendRoom
                                        |> Button.withTestId "enter-room-submit"
                                        |> Button.toHtml
                                    ]
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
                                    [ Html.h2 [ Attr.css [ Tw.mt_0, Tw.mb_4 ], Attr.attribute "data-testid" "create-story-text" ] [ text "Create new story" ] ]
                                , Html.div
                                    [ Attr.css
                                        [ Bp.sm
                                            [ Tw.mx_auto
                                            , Tw.w_full
                                            , Tw.max_w_sm
                                            ]
                                        ]
                                    ]
                                    [ Html.p [ Attr.css [ Tw.text_color Tw.gray_400, Tw.text_lg, Tw.italic, Tw.mb_4, Tw.mt_0, Tw.font_extralight ], Attr.attribute "data-testid" "create-story-info-text" ] [ text "[ Add multiple or one story ]" ]
                                    , Input.new
                                        |> Input.withHandleInput
                                            StoreStory
                                            (case model.story of
                                                Story _ storyName ->
                                                    Util.fromValidTextFieldToString storyName

                                                _ ->
                                                    ""
                                            )
                                        |> Input.withPlaceholder "eq: Task 123 ?"
                                        |> Input.withPrimaryStyles
                                        |> Input.withError model.error
                                        |> Input.withTestId "create-story-input"
                                        |> Input.toHtml
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
                                    [ Button.new
                                        |> Button.withText "Add New"
                                        |> Button.withPrimaryStyle
                                        |> Button.withOnClick SendStory
                                        |> Button.withTestId "create-story-add"
                                        |> Button.toHtml
                                    , Button.new
                                        |> Button.withText "Save"
                                        |> Button.withPrimaryStyle
                                        |> Button.withOnClick SaveStory
                                        |> Button.withTestId "create-story-submit"
                                        |> Button.toHtml
                                    ]
                                ]

                        StoryPointsSequenceStep ->
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
                                    [ Html.h2 [ Attr.css [ Tw.mt_0, Tw.mb_4 ] ] [ text "Story points sequence" ] ]
                                , Html.div
                                    [ Attr.css
                                        [ Bp.sm
                                            [ Tw.mx_auto
                                            , Tw.w_full
                                            , Tw.max_w_lg
                                            ]
                                        ]
                                    ]
                                    [ Html.p
                                        [ Attr.css
                                            [ Tw.text_color Tw.gray_400
                                            , Tw.text_lg
                                            , Tw.italic
                                            , Tw.mb_4
                                            , Tw.mt_0
                                            , Tw.font_extralight
                                            , if model.shouldEnableCustomSequence then
                                                Tw.hidden

                                              else
                                                Tw.block
                                            , Bp.lg [ Tw.block ]
                                            ]
                                        ]
                                        [ text "[ Choose between common ]" ]
                                    , Html.div
                                        [ Attr.css
                                            [ Tw.relative
                                            , Tw.flex_col
                                            , if model.shouldEnableCustomSequence then
                                                Tw.hidden

                                              else
                                                Tw.flex
                                            , Bp.lg [ Tw.flex ]
                                            ]
                                        ]
                                        [ if model.shouldEnableCustomSequence then
                                            -- Overlay el
                                            Html.div
                                                [ Attr.css
                                                    [ Tw.absolute
                                                    , Tw.w_full
                                                    , Tw.h_full
                                                    , Tw.top_0
                                                    , Tw.opacity_80
                                                    , Tw.bg_color Tw.black
                                                    , Tw.z_10
                                                    ]
                                                ]
                                                []

                                          else
                                            text ""
                                        , Html.ul [ Attr.css [ Tw.text_color Tw.white, Tw.list_none, Tw.flex, Tw.flex_wrap, Tw.items_center, Tw.p_0, Tw.mx_auto ] ]
                                            (Util.getCommonSequenceConfig model.chooseSequence
                                                |> List.map
                                                    (\config ->
                                                        viewCommonSequence config
                                                    )
                                            )
                                        , Html.div [ Attr.css [ Tw.my_4 ] ]
                                            [ Button.new
                                                |> Button.withText "Choose"
                                                |> Button.withPrimaryStyle
                                                |> Button.withOnClick SendSequence
                                                |> Button.toHtml
                                            ]
                                        ]
                                    , Html.p [ Attr.css [ Tw.text_color Tw.gray_400, Tw.text_lg, Tw.italic, Tw.mb_4, Tw.mt_0, Tw.font_extralight ] ]
                                        [ text "[ add your own sequence and press 'Enter' ]", Html.input [ onClick EnableSequenceInput, Attr.type_ "checkbox", Attr.css [ Tw.form_checkbox, Tw.cursor_pointer, Tw.text_color Tw.teal_400, Tw.bg_color Tw.transparent, Tw.p_3, Tw.ml_3, Tw.border, Tw.border_solid, Tw.border_color Tw.teal_400, Css.focus [ Tw.outline_0, Tw.ring_color Tw.transparent, Tw.ring_0, Tw.ring_offset_0 ] ] ] [] ]
                                    , viewCustomSequence { sequence = model.sequence, sequenceAsInput = model.sequenceAsInput, error = model.error, shouldEnableCustomSequence = model.shouldEnableCustomSequence }
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
                                                [ case model.editedRoomName of
                                                    UnwrappedEditState ->
                                                        Tw.w_full

                                                    WrappedEditState ->
                                                        Tw.w_auto
                                                ]
                                            ]
                                            [ case model.credentials of
                                                Admin ->
                                                    case model.editedRoomName of
                                                        UnwrappedEditState ->
                                                            Html.div [ Attr.css [ Tw.flex, Tw.items_center, Tw.self_start ] ]
                                                                [ Input.new
                                                                    |> Input.withHandleInput
                                                                        StoreRoom
                                                                        model.roomName
                                                                    |> Input.withSendOnEnter SendEditedRoom
                                                                    |> Input.withEditStyle
                                                                    |> Input.withError model.error
                                                                    |> Input.toHtml
                                                                , Button.new
                                                                    |> Button.withText "Save"
                                                                    |> Button.withEditStyle
                                                                    |> Button.withOnClick SendEditedRoom
                                                                    |> Button.toHtml
                                                                ]

                                                        WrappedEditState ->
                                                            Html.h2
                                                                [ Attr.class "edit"
                                                                , Attr.css [ Tw.m_0, Tw.break_all, Tw.flex, Tw.items_center, Tw.cursor_pointer, Tw.pl_2 ]
                                                                , onClick <| EditRoomName
                                                                ]
                                                                [ model.roomName |> text
                                                                , Html.span [ Attr.css [ Tw.flex, Tw.self_stretch, Tw.items_center, Tw.ml_2 ] ] [ Svgs.withOverrideStyle |> Svgs.iconPencil ]
                                                                ]

                                                Employee ->
                                                    Html.h2 [ Attr.css [ Tw.m_0, Tw.break_all, Tw.flex, Tw.items_center ] ] [ model.roomName |> text ]
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
                                                            , onClick <| ShowChart Bar
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
                                                            , onClick <| ShowChart Donut
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
                                            |> Maybe.withDefault (Story -1 <| ValidTextField "No more stories")
                                            |> (\story ->
                                                    case story of
                                                        Story _ storyName ->
                                                            text <| "[ Current story ] " ++ Util.fromValidTextFieldToString storyName

                                                        NoStory ->
                                                            text ""

                                                        Edit _ _ ->
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
                                    , viewButtonCommandLine model
                                    , Html.div [ Attr.css [ Tw.mt_10, Tw.flex, Tw.gap_4, Tw.items_start ] ]
                                        [ Html.h4 [ Attr.css [ Tw.text_3xl, Tw.m_0, Tw.mb_4 ] ] [ text "Stories:" ]
                                        , case model.credentials of
                                            Admin ->
                                                Button.new
                                                    |> Button.withText "+ 1"
                                                    |> Button.withPrimaryStyle
                                                    |> Button.withOnClick AddStory
                                                    |> Button.toHtml

                                            Employee ->
                                                text ""
                                        ]
                                    , viewEditStoriesSection model
                                    ]
                                , viewSidebar model
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


viewCommonSequence : SequenceConfig -> Html FrontendMsg
viewCommonSequence { msg, choosenSequence, sequenceValue, msgAttribute, borderSetup, textValue } =
    Html.li [ onClick <| msg msgAttribute, Attr.css <| [ Tw.w_2over4, Tw.border_2, Tw.border_color Tw.white, Tw.border_solid ] ++ borderSetup ]
        [ Html.div
            [ Attr.css [ Tw.m_4, Tw.p_2, Tw.bg_color Tw.teal_400, Tw.opacity_70, Tw.cursor_pointer, Tw.transition_all, Bp.md [ Tw.m_5, Tw.p_4 ], Css.hover [ Tw.opacity_100 ] ]
            , Attr.class
                (if choosenSequence == msgAttribute then
                    "selectedSequence"

                 else
                    ""
                )
            ]
            [ Html.h3 [ Attr.css [ Tw.my_2, Bp.md [ Tw.my_4 ] ] ] [ text textValue ]
            , Html.div [ Attr.css [ Tw.flex, Tw.flex_wrap ] ]
                (sequenceValue
                    |> String.split ","
                    |> List.map
                        (\intAsStr ->
                            Html.p
                                [ Attr.css
                                    [ Tw.flex
                                    , Tw.m_0
                                    , Tw.h_10
                                    , Tw.border_color Tw.white
                                    , Tw.border_solid
                                    , Tw.border_2
                                    , Tw.justify_center
                                    , Tw.w_1over4
                                    , Tw.items_center
                                    , Tw.p_0
                                    , Tw.overflow_hidden
                                    , Tw.relative
                                    , Tw.cursor_pointer
                                    ]
                                ]
                                [ text intAsStr ]
                        )
                )
            ]
        ]


viewCustomSequence : { sequence : Sequence, sequenceAsInput : String, error : InvalidTextFiled, shouldEnableCustomSequence : Bool } -> Html FrontendMsg
viewCustomSequence { sequence, sequenceAsInput, error, shouldEnableCustomSequence } =
    Html.div
        [ Attr.css [ Tw.relative ] ]
        [ Html.div
            [ Attr.css
                [ Tw.max_w_sm
                , Tw.mx_auto
                , Tw.relative
                ]
            ]
            [ Input.new
                |> Input.withHandleInput
                    StoreSequence
                    sequenceAsInput
                |> Input.withSendOnEnter CheckSequence
                |> Input.withPlaceholder "eg: 23 47 86 21 90"
                |> Input.withPrimaryStyles
                |> Input.withError error
                |> Input.toHtml
            , if shouldEnableCustomSequence then
                Html.div
                    [ Attr.css
                        [ Tw.flex
                        , Tw.opacity_80
                        , Tw.rounded
                        , Tw.text_xs
                        , Tw.text_color Tw.white
                        , Tw.flex_col
                        , Tw.mt_4
                        , Tw.border
                        , Tw.border_color Tw.white
                        , Tw.border_solid
                        , Tw.p_2
                        , Bp.lg
                            [ Tw.absolute
                            , Tw.neg_right_2over3
                            , Tw.neg_top_2over4
                            , Tw.mt_0
                            ]
                        ]
                    ]
                    [ Html.p [ Attr.css [ Tw.mt_0 ] ] [ text "It comes with couple of constrains:" ]
                    , Html.ul [ Attr.css [ Tw.list_none, Tw.p_0, Tw.m_0, Tw.text_left ] ]
                        [ Html.li [] [ text """- Enter 12 numbers separated by " " (space)""" ]
                        , Html.li [] [ text """- 3 digit max per item """ ]
                        ]
                    , Html.div
                        [ Attr.css
                            [ Tw.w_3
                            , Tw.h_0_dot_5
                            , Tw.hidden
                            , Tw.bg_color Tw.white
                            , Tw.absolute
                            , Tw.top_2over4
                            , Tw.neg_left_3
                            , Bp.lg
                                [ Tw.block
                                ]
                            ]
                        ]
                        []
                    ]

              else
                text ""
            ]
        , Html.div
            [ Attr.css
                [ Tw.mt_5
                , Bp.sm
                    [ Tw.mx_auto
                    , Tw.w_full
                    , Tw.max_w_3xl
                    ]
                ]
            ]
            [ case sequence of
                Accept input ->
                    Html.div []
                        [ Html.div [ Attr.css [ Tw.text_color Tw.gray_400, Tw.text_lg, Tw.italic, Tw.mb_4, Tw.mt_0, Tw.font_extralight, Tw.flex, Tw.justify_center ] ]
                            [ Html.div
                                [ Attr.css [ Tw.p_4, Tw.bg_color Tw.teal_400, Tw.text_color Tw.white, Tw.cursor_pointer, Tw.transition_all ]
                                ]
                                [ Html.p [ Attr.css [ Tw.mt_0 ] ] [ text "Your Sequence" ]
                                , Html.div
                                    [ Attr.css [ Tw.flex, Tw.flex_wrap ] ]
                                    (input
                                        |> String.split " "
                                        |> List.map
                                            (\intAsStr ->
                                                Html.p
                                                    [ Attr.css
                                                        [ Tw.flex
                                                        , Tw.m_0
                                                        , Tw.h_5
                                                        , Tw.border_color Tw.white
                                                        , Tw.border_solid
                                                        , Tw.border_2
                                                        , Tw.justify_center
                                                        , Tw.w_1over4
                                                        , Tw.items_center
                                                        , Tw.p_0
                                                        , Tw.text_sm
                                                        , Tw.overflow_hidden
                                                        , Tw.relative
                                                        , Tw.cursor_pointer
                                                        ]
                                                    ]
                                                    [ text intAsStr ]
                                            )
                                    )
                                ]
                            ]
                        , Button.new
                            |> Button.withText "Create"
                            |> Button.withPrimaryStyle
                            |> Button.withOnClick SendCustomSequence
                            |> Button.toHtml
                        ]

                InTransition str ->
                    let
                        numbersUntilValidSequence =
                            str |> String.split " " |> List.length |> (-) 12
                    in
                    Html.p [ Attr.css [ Tw.text_color Tw.orange_400 ] ] [ text <| String.fromInt numbersUntilValidSequence ++ " " ++ Util.pluralize (toFloat numbersUntilValidSequence) "number" ++ " to go" ]

                Reject ->
                    text ""
            ]
        , if not <| shouldEnableCustomSequence then
            -- Overlay el
            Html.div
                [ Attr.css
                    [ Tw.absolute
                    , Tw.w_full
                    , Tw.h_full
                    , Tw.top_0
                    , Tw.opacity_80
                    , Tw.bg_color Tw.black
                    ]
                ]
                []

          else
            text ""
        ]


viewEditStoriesSection : FrontendModel -> Html FrontendMsg
viewEditStoriesSection { credentials, stories, story, error } =
    Html.ul [ Attr.css [ Tw.list_none, Tw.flex, Tw.p_0, Tw.m_0, Tw.flex_col, Tw.text_2xl, Tw.gap_2 ] ]
        (stories
            |> List.map
                (\str ->
                    case credentials of
                        Admin ->
                            case str of
                                Edit storyId _ ->
                                    Html.li
                                        [ Attr.css
                                            [ Tw.flex
                                            , Tw.items_center
                                            , Tw.self_start
                                            , Tw.w_full
                                            ]
                                        ]
                                        [ Html.div [ Attr.css [ Tw.flex, Tw.items_center, Tw.self_start, Tw.w_full ] ]
                                            [ Input.new
                                                |> Input.withHandleInput
                                                    StoreEditStory
                                                    (case story of
                                                        Edit _ sn ->
                                                            Util.fromValidTextFieldToString sn

                                                        _ ->
                                                            ""
                                                    )
                                                |> Input.withSendOnEnter (SendEditedStory storyId)
                                                |> Input.withEditStyle
                                                |> Input.withError error
                                                |> Input.toHtml
                                            , Button.new
                                                |> Button.withText "Save"
                                                |> Button.withEditStyle
                                                |> Button.withOnClick (SendEditedStory storyId)
                                                |> Button.toHtml
                                            ]
                                        ]

                                Story storyId storyName ->
                                    Html.li
                                        [ Attr.css
                                            [ Tw.flex
                                            , Tw.items_center
                                            , Tw.self_start
                                            , Tw.w_auto
                                            ]
                                        ]
                                        [ Html.div [ Attr.class "edit", Attr.css [ Tw.break_all, Tw.flex, Tw.items_center, Tw.cursor_pointer, Tw.self_start, Tw.pl_2, Tw.w_full, Tw.h_full ], onClick <| EditStory storyId storyName ]
                                            [ text <| Util.fromValidTextFieldToString storyName
                                            , Html.span [ Attr.css [ Tw.flex, Tw.self_stretch, Tw.ml_2, Tw.items_center, Tw.py_1 ] ] [ Svgs.withOverrideStyle |> Svgs.iconPencil ]
                                            ]
                                        ]

                                NoStory ->
                                    Html.li [] [ text "" ]

                        Employee ->
                            let
                                -- @TODO: wtf ?
                                storyName =
                                    case str of
                                        Story _ sn ->
                                            Util.fromValidTextFieldToString sn

                                        _ ->
                                            ""
                            in
                            Html.li [ Attr.css [ Tw.break_all, Tw.py_2 ] ]
                                [ text storyName ]
                )
        )


viewButtonCommandLine : FrontendModel -> Html FrontendMsg
viewButtonCommandLine { credentials, shouldShowCharts, stories, shouldStartClock, users, shouldFlipCards } =
    Html.div [ Attr.css [ Tw.mt_10 ] ]
        [ case credentials of
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
                    [ if not <| shouldShowCharts && List.length stories > 0 then
                        if shouldStartClock then
                            Html.div
                                [ Attr.css
                                    [ Tw.flex
                                    , Tw.gap_4
                                    , Tw.flex_wrap
                                    , Tw.justify_center
                                    , Bp.lg [ Tw.justify_start ]
                                    ]
                                ]
                                [ Button.new
                                    |> Button.withText "Reset timer"
                                    |> Button.withPrimaryStyle
                                    |> Button.withOnClick ResetTime
                                    |> Button.toHtml
                                , if users |> List.any (\user -> user.voteState /= NotVoted) then
                                    if shouldFlipCards then
                                        Button.new
                                            |> Button.withText "Hide Votes "
                                            |> Button.withPrimaryStyle
                                            |> Button.withOnClick HideCards
                                            |> Button.toHtml

                                    else
                                        Button.new
                                            |> Button.withText "Show votes"
                                            |> Button.withPrimaryStyle
                                            |> Button.withOnClick ShowCards
                                            |> Button.toHtml

                                  else
                                    text ""
                                , Button.new
                                    |> Button.withText "Clear votes"
                                    |> Button.withPrimaryStyle
                                    |> Button.withOnClick ClearVotes
                                    |> Button.toHtml
                                , Button.new
                                    |> Button.withText "Skip story"
                                    |> Button.withPrimaryStyle
                                    |> Button.withOnClick SkipStory
                                    |> Button.toHtml
                                , if users |> List.all (\user -> not <| user.voteState == NotVoted) then
                                    Button.new
                                        |> Button.withText "Finish Voting"
                                        |> Button.withPrimaryStyle
                                        |> Button.withOnClick FinishVoting
                                        |> Button.toHtml

                                  else
                                    text ""
                                ]

                        else
                            Button.new
                                |> Button.withText "Start timer"
                                |> Button.withPrimaryStyle
                                |> Button.withOnClick StartTime
                                |> Button.toHtml

                      else
                        text ""
                    ]

            Employee ->
                text ""
        ]


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
    , Html.p [ Attr.css [ Tw.m_0 ] ] [ text <| Util.fromValidTextFieldToString name ]
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
            (Util.fromSequenceToCards model.chooseSequence
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


viewSidebar : FrontendModel -> Html FrontendMsg
viewSidebar { clock, url, roomId, users } =
    Html.div
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
            [ Html.div [ Attr.css [ Tw.text_5xl ] ] [ text <| Util.fromIntToCounter clock ]
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
                    , Attr.value <| url ++ "invite/" ++ (roomId |> Maybe.withDefault 1 |> String.fromInt)
                    ]
                    []
                ]
            , Button.new
                |> Button.withText "Copy URL"
                |> Button.withReadOnlyInputStyle
                |> Button.withOnClick CopyRoomUrl
                |> Button.toHtml
            , Html.div [ Attr.css [ Tw.mt_6 ] ]
                [ Html.h4 [ Attr.css [ Tw.text_3xl, Tw.m_0, Tw.mb_4 ] ] [ text "Team:" ]
                , Html.ul [ Attr.css [ Tw.list_none, Tw.flex, Tw.p_0, Tw.m_0, Tw.flex_col, Tw.text_2xl, Tw.gap_4 ] ]
                    (users
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
                                                        , Html.span [ Attr.css [ Css.minWidth (Css.px 123) ] ] [ text <| "(" ++ (entry.numOfVoters |> String.fromFloat) ++ " " ++ Util.pluralize entry.numOfVoters "player" ++ ")" ]
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
                                                            , Html.span [ Attr.css [ Css.minWidth (Css.px 123) ] ] [ text <| "(" ++ (entry.numOfVoters |> String.fromFloat) ++ " " ++ Util.pluralize entry.numOfVoters "player" ++ ")" ]
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
            Button.new
                |> Button.withText "Next Story"
                |> Button.withPrimaryStyle
                |> Button.withOnClick NextStory
                |> Button.toHtml

          else if model.shouldShowCharts && List.length model.stories == 1 && model.credentials == Admin then
            Html.div [ Attr.css [ Tw.bg_color Tw.orange_400, Tw.p_2, Tw.rounded, Tw.flex, Tw.gap_3 ] ] [ Html.span [ Attr.css [ Tw.border_solid, Tw.text_center, Tw.w_5, Tw.h_5, Tw.border, Tw.border_color Tw.white, Tw.rounded_full ] ] [ text "!" ], text """ [ Seems you are on the last story, add more on "+1" ]""" ]

          else
            text ""
        ]


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
            , Attr.attribute "data-testid" "error-message"
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
