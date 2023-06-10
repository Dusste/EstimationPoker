module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Lamdera exposing (ClientId, SessionId)
import Task exposing (sequence)
import Time
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , url : String
    , credentials : Credentials
    , status : Status
    , name : Maybe String
    , roomName : Maybe String
    , editRoomName : Maybe String
    , story : Story
    , stories : List Story
    , error : InvalidTextFiled
    , roomId : Maybe RoomParam
    , users : List User
    , sessionId : Maybe SessionId
    , clock : Int
    , storyCount : Int
    , editedStory : Story
    , sequence : Maybe String
    , chooseSequence : CommonSequence
    , shouldStartClock : Bool
    , shouldFlipCards : Bool
    , chart : Chart
    , shouldShowCharts : Bool
    , card : Maybe String
    , shouldStartChartAnimation : Bool
    , announcement : List String
    }


type alias BackendModel =
    { rooms : Dict Index Room
    , index : Index
    }


type alias Room =
    { users : List User
    , roomName : ValidTextField
    , stories : List Story
    , sequence : CommonSequence
    }


type alias User =
    { name : ValidTextField
    , isAdmin : Bool
    , card : Maybe Float
    , sessionId : SessionId
    , voteState : VoteState
    }


type Story
    = Story StoryId StoryName
    | NoStory InvalidTextFiled


type alias StoryId =
    Int


type alias StoryName =
    ValidTextField


type CommonSequence
    = Default
    | Option2
    | Option3
    | Option4


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
    | StoryPointsSequenceStep
    | PokerStep
    | Step404


type alias Index =
    Int


type VoteState
    = NotVoted
    | HiddenVote
    | Voted


type Credentials
    = Admin
    | Employee


type alias ChartsData =
    { uniqueVoteValue : Maybe Float
    , percentage : Float
    , numOfVoters : Float
    }


defaultRoom : Room
defaultRoom =
    { users = []
    , roomName = "Room name not availabe"
    , stories = []
    , sequence = Default
    }


defaultUser : User
defaultUser =
    { name = "Name not available"
    , isAdmin = False
    , card = Nothing
    , sessionId = "Invalid session id"
    , voteState = NotVoted
    }


type alias SequenceString =
    String


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | StoreName String
    | StoreRoom String
    | StoreStory String
    | SendName Credentials
    | SendRoom
    | SendEditedRoom
    | SendStory
    | SendEditedStory Int
    | SaveStory
    | ChooseCard Float String
    | Tick Time.Posix
    | StartTime
    | ResetTime
    | ShowCards
    | HideCards
    | ClearVotes
    | StoreSequence SequenceString
    | SendSequence
    | SelectSequence CommonSequence
    | FinishVoting
    | NextStory
    | SkipStory
    | StartChartAnimation
    | ShowDonutChart
    | ShowBarChart
    | HideNotification
    | CopyRoomUrl
    | EditStory StoryId StoryName
    | EditRoomName ValidTextField
    | AddStory
    | NoOp


type Chart
    = Donut
    | Bar


type ToBackend
    = SendStoryToBE (List Story) RoomParam
    | SendRoomNameToBE ValidTextField RoomParam
    | SendAdminNameToBE ValidTextField
    | SendUserNameToBE ValidTextField RoomParam
    | ReqRoomRoute RoomParam Credentials
    | SendCard Float RoomParam
    | StartTimerAndVote RoomParam
    | ResetTimerAndVote RoomParam
    | InitiateShowCards RoomParam
    | InitiateHideCards RoomParam
    | ClearAllUserVotes RoomParam
    | SignalShowCharts RoomParam
    | SignalSkipStory RoomParam
    | SignalUpdateStories (List Story) RoomParam
    | SignalChartAnimation RoomParam
    | SignalRoomNameEdit ValidTextField RoomParam
    | SendSequenceToBE CommonSequence RoomParam


type BackendMsg
    = UserConnected SessionId ClientId
    | UserDisconnected SessionId ClientId


type ToFrontend
    = SendRoomIdToFE RoomParam
    | ResRoomRoute { status : Status, roomName : ValidTextField, sessionId : SessionId, users : List User, stories : List Story }
    | UpdateRoom { sessionId : SessionId, name : ValidTextField }
    | SupplyBEData { users : List User, stories : List Story }
    | UsersStartTimer
    | UsersResetTimer
    | UpdateCards (List User)
    | UsersFlipCards (List User)
    | UsersCardReset (List User)
    | SkipStoryAndExposeCharts (List User)
    | ExposeCharts
    | UpdateStoriesAfterSkip (List Story) (List User)
    | UpdateStories (List Story)
    | ChartAnimation
    | UpdateRoomName ValidTextField
    | UpdateSequence CommonSequence
