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
import Tailwind.Breakpoints as Bp
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
                                    , Html.input
                                        [ Attr.type_ "text"
                                        , Attr.css
                                            [ Tw.block
                                            , Tw.w_full
                                            , Tw.form_input
                                            , Tw.rounded_md
                                            , Tw.border_0
                                            , Tw.py_1_dot_5
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
                                        , onInput StoreName
                                        , model.name
                                            |> Maybe.withDefault ""
                                            |> Attr.value
                                        ]
                                        []
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
                                    [ Html.button
                                        [ onClick <| SendName Admin
                                        , Attr.css
                                            [ Tw.bg_color Tw.transparent
                                            , Tw.text_color Tw.white
                                            , Tw.py_2
                                            , Tw.px_4
                                            , Tw.text_2xl
                                            , Tw.border
                                            , Tw.border_color Tw.white
                                            , Tw.rounded
                                            , Tw.cursor_pointer
                                            , Css.hover
                                                [ Tw.bg_color Tw.white
                                                , Tw.text_color Tw.black
                                                , Tw.border_color Tw.transparent
                                                ]
                                            ]
                                        ]
                                        [ text "Save" ]
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
                                    , Html.input
                                        [ Attr.type_ "text"
                                        , Attr.css
                                            [ Tw.block
                                            , Tw.w_full
                                            , Tw.form_input
                                            , Tw.rounded_md
                                            , Tw.border_0
                                            , Tw.py_1_dot_5
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
                                        , onInput StoreName
                                        , model.name
                                            |> Maybe.withDefault ""
                                            |> Attr.value
                                        ]
                                        []
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
                                    [ Html.button
                                        [ Attr.css
                                            [ Tw.bg_color Tw.transparent
                                            , Tw.text_color Tw.white
                                            , Tw.py_2
                                            , Tw.px_4
                                            , Tw.text_2xl
                                            , Tw.border
                                            , Tw.transition_all
                                            , Tw.duration_300
                                            , Tw.border_color Tw.white
                                            , Tw.rounded
                                            , Tw.cursor_pointer
                                            , Css.hover
                                                [ Tw.bg_color Tw.white
                                                , Tw.text_color Tw.black
                                                , Tw.border_color Tw.transparent
                                                ]
                                            ]
                                        , onClick <|
                                            SendName Employee
                                        ]
                                        [ text "Save" ]
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
                                    , Html.input
                                        [ Attr.type_ "text"
                                        , Attr.css
                                            [ Tw.block
                                            , Tw.w_full
                                            , Tw.form_input
                                            , Tw.rounded_md
                                            , Tw.border_0
                                            , Tw.py_1_dot_5
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
                                        , onInput StoreRoom
                                        , model.roomName
                                            |> Maybe.withDefault ""
                                            |> Attr.value
                                        ]
                                        []
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
                                    [ Html.button
                                        [ onClick SendRoom
                                        , Attr.css
                                            [ Tw.bg_color Tw.transparent
                                            , Tw.text_color Tw.white
                                            , Tw.py_2
                                            , Tw.transition_all
                                            , Tw.duration_300
                                            , Tw.px_4
                                            , Tw.text_2xl
                                            , Tw.border
                                            , Tw.border_color Tw.white
                                            , Tw.rounded
                                            , Tw.cursor_pointer
                                            , Css.hover
                                                [ Tw.bg_color Tw.white
                                                , Tw.text_color Tw.black
                                                , Tw.border_color Tw.transparent
                                                ]
                                            ]
                                        ]
                                        [ text "Create" ]
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
                                    , Html.input
                                        [ Attr.type_ "text"
                                        , onInput StoreStory
                                        , Attr.css
                                            [ Tw.block
                                            , Tw.w_full
                                            , Tw.form_input
                                            , Tw.rounded_md
                                            , Tw.border_0
                                            , Tw.py_1_dot_5
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
                                        , model.story
                                            |> Maybe.withDefault ""
                                            |> Attr.value
                                        ]
                                        []
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
                                    [ Html.button
                                        [ onClick SendStory
                                        , Attr.css
                                            [ Tw.bg_color Tw.transparent
                                            , Tw.text_color Tw.white
                                            , Tw.py_2
                                            , Tw.px_4
                                            , Tw.text_xl
                                            , Tw.border
                                            , Tw.mr_6
                                            , Tw.transition_all
                                            , Tw.duration_300
                                            , Tw.border_color Tw.white
                                            , Tw.rounded
                                            , Tw.cursor_pointer
                                            , Css.hover
                                                [ Tw.bg_color Tw.white
                                                , Tw.text_color Tw.black
                                                , Tw.border_color Tw.transparent
                                                ]
                                            ]
                                        ]
                                        [ text "Add new" ]
                                    , Html.button
                                        [ onClick SaveStory
                                        , Attr.css
                                            [ Tw.bg_color Tw.transparent
                                            , Tw.text_color Tw.white
                                            , Tw.py_2
                                            , Tw.px_4
                                            , Tw.text_xl
                                            , Tw.transition_all
                                            , Tw.duration_300
                                            , Tw.border
                                            , Tw.border_color Tw.white
                                            , Tw.rounded
                                            , Tw.cursor_pointer
                                            , Css.hover
                                                [ Tw.bg_color Tw.white
                                                , Tw.text_color Tw.black
                                                , Tw.border_color Tw.transparent
                                                ]
                                            ]
                                        ]
                                        [ text "Save" ]
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
                                        ]
                                    ]
                                    [ Html.div
                                        [ Attr.css
                                            [ Tw.text_color Tw.white
                                            , Tw.text_2xl
                                            , Bp.sm
                                                [ Tw.mx_auto
                                                , Tw.w_full
                                                ]
                                            ]
                                        ]
                                        [ Html.h2 [ Attr.css [ Tw.mt_0, Tw.mb_4 ] ] [ model.roomName |> Maybe.withDefault "Room name is not available" |> text ] ]
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
                                                                [ Html.button
                                                                    [ Attr.css
                                                                        [ Tw.bg_color Tw.transparent
                                                                        , Tw.text_color Tw.white
                                                                        , Tw.py_2
                                                                        , Tw.transition_all
                                                                        , Tw.duration_300
                                                                        , Tw.px_4
                                                                        , Tw.text_xl
                                                                        , Tw.border
                                                                        , Tw.border_color Tw.white
                                                                        , Tw.rounded
                                                                        , Tw.cursor_pointer
                                                                        , Css.hover
                                                                            [ Tw.bg_color Tw.white
                                                                            , Tw.text_color Tw.black
                                                                            , Tw.border_color Tw.transparent
                                                                            ]
                                                                        ]
                                                                    , onClick ResetTime
                                                                    ]
                                                                    [ text "Reset timer" ]
                                                                , Html.button
                                                                    [ Attr.css
                                                                        [ Tw.bg_color Tw.transparent
                                                                        , Tw.text_color Tw.white
                                                                        , Tw.py_2
                                                                        , Tw.px_4
                                                                        , Tw.text_xl
                                                                        , Tw.border
                                                                        , Tw.transition_all
                                                                        , Tw.duration_300
                                                                        , Tw.border_color Tw.white
                                                                        , Tw.rounded
                                                                        , Tw.cursor_pointer
                                                                        , Css.hover
                                                                            [ Tw.bg_color Tw.white
                                                                            , Tw.text_color Tw.black
                                                                            , Tw.border_color Tw.transparent
                                                                            ]
                                                                        ]
                                                                    , onClick FlipCards
                                                                    ]
                                                                    [ text "Flip cards" ]
                                                                , Html.button
                                                                    [ Attr.css
                                                                        [ Tw.bg_color Tw.transparent
                                                                        , Tw.text_color Tw.white
                                                                        , Tw.py_2
                                                                        , Tw.px_4
                                                                        , Tw.text_xl
                                                                        , Tw.transition_all
                                                                        , Tw.duration_300
                                                                        , Tw.border
                                                                        , Tw.border_color Tw.white
                                                                        , Tw.rounded
                                                                        , Tw.cursor_pointer
                                                                        , Css.hover
                                                                            [ Tw.bg_color Tw.white
                                                                            , Tw.text_color Tw.black
                                                                            , Tw.border_color Tw.transparent
                                                                            ]
                                                                        ]
                                                                    , onClick ClearVotes
                                                                    ]
                                                                    [ text "Clear votes" ]
                                                                , Html.button
                                                                    [ Attr.css
                                                                        [ Tw.bg_color Tw.transparent
                                                                        , Tw.text_color Tw.white
                                                                        , Tw.py_2
                                                                        , Tw.px_4
                                                                        , Tw.text_xl
                                                                        , Tw.border
                                                                        , Tw.transition_all
                                                                        , Tw.duration_300
                                                                        , Tw.border_color Tw.white
                                                                        , Tw.rounded
                                                                        , Tw.cursor_pointer
                                                                        , Css.hover
                                                                            [ Tw.bg_color Tw.white
                                                                            , Tw.text_color Tw.black
                                                                            , Tw.border_color Tw.transparent
                                                                            ]
                                                                        ]
                                                                    , onClick NextStory
                                                                    ]
                                                                    [ text "Skip story" ]
                                                                , if model.users |> List.all (\user -> user.hasVoted) then
                                                                    Html.button
                                                                        [ Attr.css
                                                                            [ Tw.bg_color Tw.transparent
                                                                            , Tw.text_color Tw.white
                                                                            , Tw.py_2
                                                                            , Tw.px_4
                                                                            , Tw.text_xl
                                                                            , Tw.border
                                                                            , Tw.transition_all
                                                                            , Tw.duration_300
                                                                            , Tw.border_color Tw.white
                                                                            , Tw.rounded
                                                                            , Tw.cursor_pointer
                                                                            , Css.hover
                                                                                [ Tw.bg_color Tw.white
                                                                                , Tw.text_color Tw.black
                                                                                , Tw.border_color Tw.transparent
                                                                                ]
                                                                            ]
                                                                        , onClick FinishVoting
                                                                        ]
                                                                        [ text "Finish Voting" ]

                                                                  else
                                                                    text ""
                                                                ]

                                                        else
                                                            Html.button
                                                                [ Attr.css
                                                                    [ Tw.bg_color Tw.transparent
                                                                    , Tw.text_color Tw.white
                                                                    , Tw.py_2
                                                                    , Tw.px_4
                                                                    , Tw.text_xl
                                                                    , Tw.border
                                                                    , Tw.border_color Tw.white
                                                                    , Tw.rounded
                                                                    , Tw.transition_all
                                                                    , Tw.duration_300
                                                                    , Tw.cursor_pointer
                                                                    , Css.hover
                                                                        [ Tw.bg_color Tw.white
                                                                        , Tw.text_color Tw.black
                                                                        , Tw.border_color Tw.transparent
                                                                        ]
                                                                    ]
                                                                , onClick StartTime
                                                                ]
                                                                [ text "Start timer" ]

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
                                        , Tw.mt_48
                                        , Tw.border_l
                                        , Tw.border_color Tw.teal_400
                                        , Tw.border_solid
                                        , Tw.border_r_0
                                        , Tw.border_b_0
                                        , Tw.border_t_0
                                        , Tw.pl_10
                                        ]
                                    ]
                                    [ Html.div [ Attr.css [ Tw.text_5xl ] ] [ text <| Util.fromIntToCounter model.clock ]
                                    , Html.p [ Attr.css [ Tw.text_2xl, Tw.text_color Tw.gray_400 ] ] [ text "[ Copy link and send to collegues ]" ]
                                    , Html.div []
                                        [ Html.input
                                            [ Attr.readonly True
                                            , Attr.css
                                                [ Tw.block
                                                , Tw.w_full
                                                , Tw.border_color Tw.white
                                                , Tw.rounded_md
                                                , Tw.py_2
                                                , Tw.pl_3
                                                , Tw.pr_3
                                                , Tw.text_color Tw.white
                                                , Tw.shadow_sm
                                                , Tw.bg_color Tw.black
                                                , Tw.font_mono
                                                , Tw.text_color Tw.teal_400
                                                , Tw.text_right
                                                , Css.focus
                                                    [ Tw.outline_none
                                                    ]
                                                , Bp.sm
                                                    [ Tw.text_sm
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
    , { name = 0x2615 |> Char.fromCode |> String.fromChar, value = 0 }
    ]


viewCards : FrontendModel -> Html FrontendMsg
viewCards model =
    Html.div []
        [ Html.h3 [ Attr.css [ Tw.text_color Tw.gray_400, Tw.font_light ] ] [ text "[ Pick card to estimate story ]" ]
        , Html.ul [ Attr.css [ Tw.list_none, Tw.flex, Tw.flex_wrap, Tw.p_0, Tw.m_0, Tw.gap_10, Tw.text_2xl ] ]
            (cards
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
