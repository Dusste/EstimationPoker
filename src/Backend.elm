module Backend exposing (..)

import Dict
import Lamdera exposing (ClientId, SessionId, broadcast, onConnect, onDisconnect, sendToBackend, sendToFrontend)
import Types exposing (..)


type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = subscriptions
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { rooms = Dict.empty, index = 1 }
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        UserDisconnected sessionId clientId ->
            ( model, Cmd.none )

        UserConnected sessionId clientId ->
            ( model, Cmd.none )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        SendAdminNameToBE adminName ->
            let
                initiateNewRoom =
                    Dict.insert model.index
                        { users =
                            [ { defaultUser
                                | sessionId = sessionId
                                , name = adminName
                                , isAdmin = True
                              }
                            ]
                        , roomName = ""
                        , stories = []
                        , sequence = Default
                        }
                        model.rooms
            in
            ( { model | rooms = initiateNewRoom, index = model.index + 1 }
            , sendToFrontend sessionId <| SendRoomIdToFE model.index
            )

        SendUserNameToBE userName roomId ->
            let
                updateRecord =
                    Maybe.map (\room -> { room | users = { defaultUser | sessionId = sessionId, name = userName } :: room.users })

                updateRooms =
                    Dict.update roomId updateRecord model.rooms

                { users, stories } =
                    updateRooms
                        |> Dict.get roomId
                        |> Maybe.withDefault defaultRoom

                notifyCertainUsersAboutSomething : List User -> ({ sessionId : SessionId, name : ValidTextField } -> toFrontend) -> (SessionId -> toFrontend -> Cmd backendMsg) -> List (Cmd backendMsg)
                notifyCertainUsersAboutSomething usrs msgToFe f =
                    case usrs of
                        [] ->
                            []

                        x :: xs ->
                            (f x.sessionId <| msgToFe { sessionId = sessionId, name = userName }) :: notifyCertainUsersAboutSomething xs msgToFe f
            in
            ( { model | rooms = updateRooms }
            , Cmd.batch ([ sendToFrontend sessionId <| SupplyBEData { users = users, stories = stories } ] ++ notifyCertainUsersAboutSomething users UpdateRoom sendToFrontend)
            )

        SendRoomNameToBE roomName roomId ->
            --passing room id back and forth as a key for updating Dict
            let
                updateRecord =
                    Maybe.map (\room -> { room | roomName = roomName })

                updateRooms =
                    Dict.update roomId updateRecord model.rooms
            in
            ( { model | rooms = updateRooms }, Cmd.none )

        SendStoryToBE stories roomId ->
            let
                updateRecord =
                    Maybe.map (\room -> { room | stories = stories })

                updateRooms =
                    Dict.update roomId updateRecord model.rooms

                { users } =
                    updateRooms
                        |> Dict.get roomId
                        |> Maybe.withDefault defaultRoom

                notifyCertainUsersAboutSomething : List User -> (List Story -> toFrontend) -> (SessionId -> toFrontend -> Cmd backendMsg) -> List (Cmd backendMsg)
                notifyCertainUsersAboutSomething usrs msgToFe f =
                    case usrs of
                        [] ->
                            []

                        x :: xs ->
                            (f x.sessionId <| msgToFe stories) :: notifyCertainUsersAboutSomething xs msgToFe f
            in
            ( { model | rooms = updateRooms }, notifyCertainUsersAboutSomething users UpdateStories sendToFrontend |> Cmd.batch )

        ReqRoomRoute roomId credentials ->
            let
                maybeRoom =
                    model.rooms |> Dict.get roomId
            in
            case maybeRoom of
                Just room ->
                    let
                        { roomName, users, stories } =
                            room

                        fromBEtoFEmodel =
                            { status =
                                case credentials of
                                    Admin ->
                                        PokerStep

                                    Employee ->
                                        EnterNameStep
                            , roomName = roomName
                            , sessionId = sessionId
                            , users = users
                            , stories = stories
                            }
                    in
                    ( model, sendToFrontend sessionId <| ResRoomRoute fromBEtoFEmodel )

                Nothing ->
                    ( model, Cmd.none )

        SendCard cardValue roomId ->
            let
                updateUser usrs =
                    usrs
                        |> List.map
                            (\user ->
                                if user.sessionId == sessionId then
                                    { user | card = Just cardValue, voteState = HiddenVote }

                                else
                                    user
                            )

                updateRecord =
                    Maybe.map (\room -> { room | users = updateUser room.users })

                updateRooms =
                    Dict.update roomId updateRecord model.rooms

                { users } =
                    updateRooms
                        |> Dict.get roomId
                        |> Maybe.withDefault defaultRoom

                notifyCertainUsersAboutSomething : List User -> (List User -> toFrontend) -> (SessionId -> toFrontend -> Cmd backendMsg) -> List (Cmd backendMsg)
                notifyCertainUsersAboutSomething usrs msgToFe f =
                    case usrs of
                        [] ->
                            []

                        x :: xs ->
                            (f x.sessionId <| msgToFe users) :: notifyCertainUsersAboutSomething xs msgToFe f
            in
            ( { model | rooms = updateRooms }, notifyCertainUsersAboutSomething users UpdateCards sendToFrontend |> Cmd.batch )

        StartTimerAndVote roomId ->
            let
                { users } =
                    model.rooms
                        |> Dict.get roomId
                        |> Maybe.withDefault defaultRoom

                notifyCertainUsersAboutSomething : List User -> toFrontend -> (SessionId -> toFrontend -> Cmd backendMsg) -> List (Cmd backendMsg)
                notifyCertainUsersAboutSomething usrs msgToFe f =
                    case usrs of
                        [] ->
                            []

                        x :: xs ->
                            f x.sessionId msgToFe :: notifyCertainUsersAboutSomething xs msgToFe f
            in
            ( model, notifyCertainUsersAboutSomething users UsersStartTimer sendToFrontend |> Cmd.batch )

        ResetTimerAndVote roomId ->
            let
                updateUser usrs =
                    usrs
                        |> List.map
                            (\user ->
                                { user | voteState = NotVoted }
                            )

                updateRecord =
                    Maybe.map (\room -> { room | users = updateUser room.users })

                updateRooms =
                    Dict.update roomId updateRecord model.rooms

                { users } =
                    updateRooms
                        |> Dict.get roomId
                        |> Maybe.withDefault defaultRoom

                notifyCertainUsersAboutSomething : List User -> toFrontend -> (SessionId -> toFrontend -> Cmd backendMsg) -> List (Cmd backendMsg)
                notifyCertainUsersAboutSomething usrs msgToFe f =
                    case usrs of
                        [] ->
                            []

                        x :: xs ->
                            f x.sessionId msgToFe :: notifyCertainUsersAboutSomething xs msgToFe f
            in
            ( model, notifyCertainUsersAboutSomething users UsersResetTimer sendToFrontend |> Cmd.batch )

        InitiateShowCards roomId ->
            let
                updateUser usrs =
                    usrs
                        |> List.map
                            (\user ->
                                { user | voteState = Voted }
                            )

                updateRecord =
                    Maybe.map (\room -> { room | users = updateUser room.users })

                updateRooms =
                    Dict.update roomId updateRecord model.rooms

                { users } =
                    updateRooms
                        |> Dict.get roomId
                        |> Maybe.withDefault defaultRoom

                notifyCertainUsersAboutSomething : List User -> (List User -> toFrontend) -> (SessionId -> toFrontend -> Cmd backendMsg) -> List (Cmd backendMsg)
                notifyCertainUsersAboutSomething usrs msgToFe f =
                    case usrs of
                        [] ->
                            []

                        x :: xs ->
                            (f x.sessionId <| msgToFe users) :: notifyCertainUsersAboutSomething xs msgToFe f
            in
            ( model, notifyCertainUsersAboutSomething users UsersFlipCards sendToFrontend |> Cmd.batch )

        InitiateHideCards roomId ->
            let
                updateUser usrs =
                    usrs
                        |> List.map
                            (\user ->
                                case user.voteState of
                                    Voted ->
                                        { user | voteState = HiddenVote }

                                    NotVoted ->
                                        { user | voteState = NotVoted }

                                    HiddenVote ->
                                        { user | voteState = HiddenVote }
                            )

                updateRecord =
                    Maybe.map (\room -> { room | users = updateUser room.users })

                updateRooms =
                    Dict.update roomId updateRecord model.rooms

                { users } =
                    updateRooms
                        |> Dict.get roomId
                        |> Maybe.withDefault defaultRoom

                notifyCertainUsersAboutSomething : List User -> (List User -> toFrontend) -> (SessionId -> toFrontend -> Cmd backendMsg) -> List (Cmd backendMsg)
                notifyCertainUsersAboutSomething usrs msgToFe f =
                    case usrs of
                        [] ->
                            []

                        x :: xs ->
                            (f x.sessionId <| msgToFe users) :: notifyCertainUsersAboutSomething xs msgToFe f
            in
            ( model, notifyCertainUsersAboutSomething users UsersFlipCards sendToFrontend |> Cmd.batch )

        ClearAllUserVotes roomId ->
            let
                { users } =
                    model.rooms
                        |> Dict.get roomId
                        |> Maybe.withDefault defaultRoom

                updateUsers =
                    users
                        |> List.map
                            (\user -> { user | card = Nothing, voteState = NotVoted })

                updateRecord =
                    Maybe.map (\room -> { room | users = updateUsers })

                updateRooms =
                    Dict.update roomId updateRecord model.rooms

                notifyCertainUsersAboutSomething : List User -> (List User -> toFrontend) -> (SessionId -> toFrontend -> Cmd backendMsg) -> List (Cmd backendMsg)
                notifyCertainUsersAboutSomething usrs msgToFe f =
                    case usrs of
                        [] ->
                            []

                        x :: xs ->
                            (f x.sessionId <| msgToFe updateUsers) :: notifyCertainUsersAboutSomething xs msgToFe f
            in
            ( { model | rooms = updateRooms }, notifyCertainUsersAboutSomething updateUsers UsersCardReset sendToFrontend |> Cmd.batch )

        SignalSkipStory roomId ->
            let
                { users } =
                    model.rooms
                        |> Dict.get roomId
                        |> Maybe.withDefault defaultRoom

                updateUsers =
                    users
                        |> List.map
                            (\user -> { user | card = Nothing, voteState = NotVoted })

                updateRecord =
                    Maybe.map (\room -> { room | users = updateUsers })

                updateRooms =
                    Dict.update roomId updateRecord model.rooms

                notifyCertainUsersAboutSomething : List User -> (List User -> toFrontend) -> (SessionId -> toFrontend -> Cmd backendMsg) -> List (Cmd backendMsg)
                notifyCertainUsersAboutSomething usrs msgToFe f =
                    case usrs of
                        [] ->
                            []

                        x :: xs ->
                            (f x.sessionId <| msgToFe updateUsers) :: notifyCertainUsersAboutSomething xs msgToFe f
            in
            ( { model | rooms = updateRooms }, notifyCertainUsersAboutSomething updateUsers SkipStoryAndExposeCharts sendToFrontend |> Cmd.batch )

        SignalShowCharts roomId ->
            let
                { users } =
                    model.rooms
                        |> Dict.get roomId
                        |> Maybe.withDefault defaultRoom

                notifyCertainUsersAboutSomething : List User -> toFrontend -> (SessionId -> toFrontend -> Cmd backendMsg) -> List (Cmd backendMsg)
                notifyCertainUsersAboutSomething usrs msgToFe f =
                    case usrs of
                        [] ->
                            []

                        x :: xs ->
                            (f x.sessionId <| msgToFe) :: notifyCertainUsersAboutSomething xs msgToFe f
            in
            ( model, notifyCertainUsersAboutSomething users ExposeCharts sendToFrontend |> Cmd.batch )

        SignalChartAnimation roomId ->
            let
                { users } =
                    model.rooms
                        |> Dict.get roomId
                        |> Maybe.withDefault defaultRoom

                notifyCertainUsersAboutSomething : List User -> toFrontend -> (SessionId -> toFrontend -> Cmd backendMsg) -> List (Cmd backendMsg)
                notifyCertainUsersAboutSomething usrs msgToFe f =
                    case usrs of
                        [] ->
                            []

                        x :: xs ->
                            (f x.sessionId <| msgToFe) :: notifyCertainUsersAboutSomething xs msgToFe f
            in
            ( model, notifyCertainUsersAboutSomething users ChartAnimation sendToFrontend |> Cmd.batch )

        SignalUpdateStories updatedStories roomId ->
            let
                updateRecord =
                    Maybe.map (\room -> { room | stories = updatedStories })

                updateRooms =
                    Dict.update roomId updateRecord model.rooms

                { users } =
                    updateRooms
                        |> Dict.get roomId
                        |> Maybe.withDefault defaultRoom

                updateUsers =
                    users
                        |> List.map
                            (\user -> { user | card = Nothing, voteState = NotVoted })

                notifyCertainUsersAboutSomething : List User -> (List Story -> List User -> toFrontend) -> (SessionId -> toFrontend -> Cmd backendMsg) -> List (Cmd backendMsg)
                notifyCertainUsersAboutSomething usrs msgToFe f =
                    case usrs of
                        [] ->
                            []

                        x :: xs ->
                            (f x.sessionId <| msgToFe updatedStories updateUsers) :: notifyCertainUsersAboutSomething xs msgToFe f
            in
            ( { model | rooms = updateRooms }, notifyCertainUsersAboutSomething users UpdateStoriesAfterSkip sendToFrontend |> Cmd.batch )

        SignalRoomNameEdit roomName roomId ->
            let
                updateRecord =
                    Maybe.map (\room -> { room | roomName = roomName })

                updateRooms =
                    Dict.update roomId updateRecord model.rooms

                { users } =
                    updateRooms
                        |> Dict.get roomId
                        |> Maybe.withDefault defaultRoom

                notifyCertainUsersAboutSomething : List User -> (ValidTextField -> toFrontend) -> (SessionId -> toFrontend -> Cmd backendMsg) -> List (Cmd backendMsg)
                notifyCertainUsersAboutSomething usrs msgToFe f =
                    case usrs of
                        [] ->
                            []

                        x :: xs ->
                            (f x.sessionId <| msgToFe roomName) :: notifyCertainUsersAboutSomething xs msgToFe f
            in
            ( { model | rooms = updateRooms }, notifyCertainUsersAboutSomething users UpdateRoomName sendToFrontend |> Cmd.batch )

        SendSequenceToBE commonSequence roomId ->
            let
                updateRecord =
                    Maybe.map (\room -> { room | sequence = commonSequence })

                updateRooms =
                    Dict.update roomId updateRecord model.rooms
            in
            ( { model | rooms = updateRooms }, sendToFrontend sessionId <| UpdateSequence commonSequence )

        SendCustomSequenceToBE customSequence roomId ->
            let
                seqSeparatedByComma =
                    customSequence |> String.split "-" |> String.join ","

                updateRecord =
                    Maybe.map (\room -> { room | sequence = CustomSequence seqSeparatedByComma })

                updateRooms =
                    Dict.update roomId updateRecord model.rooms
            in
            ( { model | rooms = updateRooms }, sendToFrontend sessionId <| UpdateSequence (CustomSequence seqSeparatedByComma) )


subscriptions : BackendModel -> Sub BackendMsg
subscriptions _ =
    Sub.batch
        [ onConnect UserConnected
        , onDisconnect UserDisconnected
        ]
