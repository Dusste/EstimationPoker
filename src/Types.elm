module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Lamdera exposing (ClientId, SessionId)
import Time
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , url : String
    , credentials : Credentials
    , status : Status
    , name : Maybe String
    , roomName : Maybe String
    , story : Maybe String
    , stories : List String
    , error : InvalidTextFiled
    , roomId : Maybe Int -- index in BE model
    , users : List User
    , sessionId : Maybe SessionId
    , clock : Int
    , shouldStartClock : Bool
    , shouldFlipCards : Bool
    , chart : Chart
    , shouldShowCharts : Bool
    , card : Maybe String
    , shouldStartChartAnimation : Bool
    , announcement : List (Maybe String)
    }


type alias BackendModel =
    { rooms : Dict Index Room
    , index : Int
    }


type Route
    = InviteRoute RoomParam
    | Home
    | RoomRoute RoomParam
    | NotFound


type alias RoomParam =
    Int


type alias InvalidTextFiled =
    Maybe String


type alias ValidTextField =
    String


type Status
    = EnterNameStep
    | EnterAdminNameStep
    | CreateRoomStep
    | CreateStoryStep
    | PokerStep
    | Step404


type alias Index =
    Int


type Credentials
    = Admin
    | Employee


type alias Room =
    { users : List User
    , roomName : ValidTextField
    , stories : List ValidTextField
    }


type alias ChartsData =
    { uniqueVoteValue : Maybe Float
    , percentage : Float
    , numOfVoters : Float
    }


type alias User =
    { name : ValidTextField
    , isAdmin : Bool
    , card : Maybe Float
    , sessionId : SessionId
    , hasVoted : Bool
    }


defaultRoom : Room
defaultRoom =
    { users = []
    , roomName = "Room name not availabe"
    , stories = []
    }


defaultUser : User
defaultUser =
    { name = "Name not available"
    , isAdmin = False
    , card = Nothing
    , sessionId = "Invalid session id"
    , hasVoted = False
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
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


type Chart
    = Donut
    | Bar


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
    = UserConnected SessionId ClientId
    | UserDisconnected SessionId ClientId


type ToFrontend
    = SendRoomIdToFE Int
    | ResRoomRoute { status : Status, roomName : String, sessionId : SessionId, users : List User, stories : List ValidTextField }
    | UpdateRoom { sessionId : SessionId, name : String }
    | SupplyBEData { users : List User, stories : List ValidTextField }
    | UsersStartTimer
    | UsersResetTimer
    | UpdateCards (List User)
    | UsersFlipCards
    | UsersCardReset (List User)
    | SkipStoryAndExposeCharts (List User)
    | ExposeCharts
    | UpdateStories (List ValidTextField) (List User)
    | ChartAnimation
