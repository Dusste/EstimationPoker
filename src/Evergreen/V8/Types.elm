module Evergreen.V8.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Lamdera
import Time
import Url


type Credentials
    = Admin
    | Employee


type Status
    = EnterNameStep
    | EnterAdminNameStep
    | CreateRoomStep
    | CreateStoryStep
    | PokerStep
    | Step404


type alias InvalidTextFiled =
    Maybe String


type alias ValidTextField =
    String


type alias User =
    { name : ValidTextField
    , isAdmin : Bool
    , card : Maybe Float
    , clientId : Lamdera.ClientId
    , hasVoted : Bool
    }


type Chart
    = Donut
    | Bar


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , url : String
    , credentials : Credentials
    , status : Status
    , name : Maybe String
    , roomName : Maybe String
    , story : Maybe String
    , stories : List String
    , error : InvalidTextFiled
    , roomId : Maybe Int
    , users : List User
    , clientId : Maybe Lamdera.ClientId
    , clock : Int
    , shouldStartClock : Bool
    , shouldFlipCards : Bool
    , chart : Chart
    , shouldShowCharts : Bool
    , card : Maybe String
    , shouldStartChartAnimation : Bool
    }


type alias Index =
    Int


type alias Room =
    { users : List User
    , roomName : ValidTextField
    , stories : List ValidTextField
    }


type alias BackendModel =
    { rooms : Dict.Dict Index Room
    , index : Int
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | StoreName String
    | StoreRoom String
    | StoreStory String
    | SendName Credentials
    | SendRoom
    | SendStory
    | SaveStory
    | ChooseCard Float String
    | Tick Time.Posix
    | StartTime
    | ResetTime
    | FlipCards
    | ClearVotes
    | FinishVoting
    | NextStory
    | SkipStory
    | StartChartAnimation
    | ShowDonutChart
    | ShowBarChart


type alias RoomParam =
    Int


type ToBackend
    = SendStoryToBE (List ValidTextField) Int
    | SendRoomNameToBE ValidTextField Int
    | SendAdminNameToBE ValidTextField
    | SendUserNameToBE ValidTextField Int
    | ReqRoomRoute RoomParam Bool
    | SendCard Float Int
    | StartTimerAndVote Int
    | ResetTimerAndVote Int
    | InitiateFlipCards Int
    | ClearAllUserVotes Int
    | SignalShowCharts Int
    | SignalSkipStory Int
    | SignalUpdateStories (List ValidTextField) Int
    | SignalChartAnimation Int


type BackendMsg
    = UserConnected Lamdera.SessionId Lamdera.ClientId
    | UserDisconnected Lamdera.SessionId Lamdera.ClientId


type ToFrontend
    = SendRoomIdToFE Int
    | ResRoomRoute
        { status : Status
        , roomName : String
        , clientId : Lamdera.ClientId
        , users : List User
        , stories : List ValidTextField
        }
    | UpdateRoom
        { clientId : Lamdera.ClientId
        , name : String
        }
    | SupplyBEData
        { users : List User
        , stories : List ValidTextField
        }
    | UpdateUsers (List User)
    | UsersStartTimer
    | UsersResetTimer
    | UpdateCards (List User)
    | UsersFlipCards
    | UsersCardReset (List User)
    | SkipStoryAndExposeCharts (List User)
    | ExposeCharts
    | UpdateStories (List ValidTextField) (List User)
    | ChartAnimation
    | UserHasDisconnected Lamdera.ClientId
    | UserHasConnected Lamdera.ClientId
