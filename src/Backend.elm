module Backend exposing (..)

import Dict
import Lamdera exposing (ClientId, SessionId, onConnect, onDisconnect, sendToFrontend)
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
        UserDisconnected _ clientId ->
            let
                getRoomIdOfDisconnectedUser : Index
                getRoomIdOfDisconnectedUser =
                    model.rooms
                        |> Dict.foldr toListOfUsers []
                        |> List.filter (\( clId, _ ) -> clId == clientId)
                        |> List.head
                        |> Maybe.withDefault ( "", 0 )
                        |> Tuple.second

                toListOfUsers : Index -> Room -> List ( ClientId, Index ) -> List ( ClientId, Index )
                toListOfUsers idx room rooms =
                    let
                        userWithRoomId =
                            room.users
                                |> List.foldr (\curr sum -> ( curr.clientId, idx ) :: sum) []
                    in
                    userWithRoomId ++ rooms
            in
            if getRoomIdOfDisconnectedUser == 0 then
                ( model, Cmd.none )

            else
                let
                    updateRooms =
                        Dict.update getRoomIdOfDisconnectedUser updateRecord model.rooms

                    updateRecord =
                        Maybe.map (\room -> { room | users = room.users |> List.filter (\user -> clientId /= user.clientId) })

                    { users } =
                        updateRooms
                            |> Dict.get getRoomIdOfDisconnectedUser
                            |> Maybe.withDefault defaultRoom

                    notifyCertainUsersAboutSomething : List User -> (List User -> toFrontend) -> (ClientId -> toFrontend -> Cmd backendMsg) -> List (Cmd backendMsg)
                    notifyCertainUsersAboutSomething usrs msgToFe f =
                        case usrs of
                            [] ->
                                []

                            x :: xs ->
                                (f x.clientId <| msgToFe users) :: notifyCertainUsersAboutSomething xs msgToFe f
                in
                ( { model | rooms = updateRooms }, notifyCertainUsersAboutSomething users UpdateUsers sendToFrontend |> Cmd.batch )

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
                                | clientId = clientId
                                , name = adminName
                                , isAdmin = True
                              }
                            ]
                        , roomName = ""
                        , stories = []
                        }
                        model.rooms
            in
            ( { model | rooms = initiateNewRoom, index = model.index + 1 }
            , sendToFrontend clientId <| SendRoomIdToFE model.index
            )

        SendUserNameToBE userName roomId ->
            let
                updateRecord =
                    Maybe.map (\room -> { room | users = { defaultUser | clientId = clientId, name = userName } :: room.users })

                updateRooms =
                    Dict.update roomId updateRecord model.rooms

                { users, stories } =
                    updateRooms
                        |> Dict.get roomId
                        |> Maybe.withDefault defaultRoom

                notifyCertainUsersAboutSomething : List User -> ({ clientId : ClientId, name : ValidTextField } -> toFrontend) -> (ClientId -> toFrontend -> Cmd backendMsg) -> List (Cmd backendMsg)
                notifyCertainUsersAboutSomething usrs msgToFe f =
                    case usrs of
                        [] ->
                            []

                        x :: xs ->
                            (f x.clientId <| msgToFe { clientId = clientId, name = userName }) :: notifyCertainUsersAboutSomething xs msgToFe f
            in
            ( { model | rooms = updateRooms }
            , Cmd.batch ([ sendToFrontend clientId <| SupplyBEData { users = users, stories = stories } ] ++ notifyCertainUsersAboutSomething users UpdateRoom sendToFrontend)
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

        SendStoryToBE story roomId ->
            let
                updateRecord =
                    Maybe.map (\room -> { room | stories = story ++ room.stories })

                updateRooms =
                    Dict.update roomId updateRecord model.rooms
            in
            ( { model | rooms = updateRooms }, Cmd.none )

        ReqRoomRoute roomId shouldNavigateToEnd ->
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
                                if shouldNavigateToEnd then
                                    PokerStep

                                else
                                    EnterNameStep
                            , roomName = roomName
                            , clientId = clientId
                            , users = users
                            , stories = stories
                            }
                    in
                    ( model, sendToFrontend clientId <| ResRoomRoute fromBEtoFEmodel )

                Nothing ->
                    ( model, Cmd.none )

        SendCard cardValue roomId ->
            let
                updateUser usrs =
                    usrs
                        |> List.map
                            (\user ->
                                if user.clientId == clientId then
                                    { user | card = cardValue }

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

                notifyCertainUsersAboutSomething : List User -> (List User -> toFrontend) -> (ClientId -> toFrontend -> Cmd backendMsg) -> List (Cmd backendMsg)
                notifyCertainUsersAboutSomething usrs msgToFe f =
                    case usrs of
                        [] ->
                            []

                        x :: xs ->
                            (f x.clientId <| msgToFe users) :: notifyCertainUsersAboutSomething xs msgToFe f
            in
            ( { model | rooms = updateRooms }, notifyCertainUsersAboutSomething users UpdateCards sendToFrontend |> Cmd.batch )

        StartTimerAndVote roomId ->
            let
                { users } =
                    model.rooms
                        |> Dict.get roomId
                        |> Maybe.withDefault defaultRoom

                notifyCertainUsersAboutSomething : List User -> toFrontend -> (ClientId -> toFrontend -> Cmd backendMsg) -> List (Cmd backendMsg)
                notifyCertainUsersAboutSomething usrs msgToFe f =
                    case usrs of
                        [] ->
                            []

                        x :: xs ->
                            f x.clientId msgToFe :: notifyCertainUsersAboutSomething xs msgToFe f
            in
            ( model, notifyCertainUsersAboutSomething users UsersStartTimer sendToFrontend |> Cmd.batch )

        ResetTimerAndVote roomId ->
            let
                { users } =
                    model.rooms
                        |> Dict.get roomId
                        |> Maybe.withDefault defaultRoom

                notifyCertainUsersAboutSomething : List User -> toFrontend -> (ClientId -> toFrontend -> Cmd backendMsg) -> List (Cmd backendMsg)
                notifyCertainUsersAboutSomething usrs msgToFe f =
                    case usrs of
                        [] ->
                            []

                        x :: xs ->
                            f x.clientId msgToFe :: notifyCertainUsersAboutSomething xs msgToFe f
            in
            ( model, notifyCertainUsersAboutSomething users UsersResetTimer sendToFrontend |> Cmd.batch )

        InitiateFlipCards roomId ->
            let
                { users } =
                    model.rooms
                        |> Dict.get roomId
                        |> Maybe.withDefault defaultRoom

                notifyCertainUsersAboutSomething : List User -> toFrontend -> (ClientId -> toFrontend -> Cmd backendMsg) -> List (Cmd backendMsg)
                notifyCertainUsersAboutSomething usrs msgToFe f =
                    case usrs of
                        [] ->
                            []

                        x :: xs ->
                            f x.clientId msgToFe :: notifyCertainUsersAboutSomething xs msgToFe f
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
                            (\user -> { user | card = 0 })

                notifyCertainUsersAboutSomething : List User -> (List User -> toFrontend) -> (ClientId -> toFrontend -> Cmd backendMsg) -> List (Cmd backendMsg)
                notifyCertainUsersAboutSomething usrs msgToFe f =
                    case usrs of
                        [] ->
                            []

                        x :: xs ->
                            (f x.clientId <| msgToFe updateUsers) :: notifyCertainUsersAboutSomething xs msgToFe f
            in
            ( model, notifyCertainUsersAboutSomething updateUsers UsersCardReset sendToFrontend |> Cmd.batch )


subscriptions : BackendModel -> Sub BackendMsg
subscriptions _ =
    Sub.batch
        [ onConnect UserConnected
        , onDisconnect UserDisconnected
        ]
