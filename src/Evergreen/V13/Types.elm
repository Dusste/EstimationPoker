module Evergreen.V13.Types exposing (..)

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


type alias StoryId =
    Int


type alias ValidTextField =
    String


type alias StoryName =
    ValidTextField


type alias InvalidTextFiled =
    Maybe String


type Story
    = Story StoryId StoryName
    | NoStory InvalidTextFiled


type alias RoomParam =
    Int


type VoteState
    = NotVoted
    | HiddenVote
    | Voted


type alias User =
    { name : ValidTextField
    , isAdmin : Bool
    , card : Maybe Float
    , sessionId : Lamdera.SessionId
    , voteState : VoteState
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
    , editRoomName : Maybe String
    , story : Story
    , stories : List Story
    , error : InvalidTextFiled
    , roomId : Maybe RoomParam
    , users : List User
    , sessionId : Maybe Lamdera.SessionId
    , clock : Int
    , storyCount : Int
    , editedStory : Story
    , shouldStartClock : Bool
    , shouldFlipCards : Bool
    , chart : Chart
    , shouldShowCharts : Bool
    , card : Maybe String
    , shouldStartChartAnimation : Bool
    , announcement : List String
    }


type alias Index =
    Int


type alias Room =
    { users : List User
    , roomName : ValidTextField
    , stories : List Story
    }


type alias BackendModel =
    { rooms : Dict.Dict Index Room
    , index : Index
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | StoreName String
    | StoreRoom String
    | StoreStory String
    | SendName Credentials
    | SendRoom
    | SendEditedRoom
    | SendStory Int
    | SaveStory
    | ChooseCard Float String
    | Tick Time.Posix
    | StartTime
    | ResetTime
    | ShowCards
    | HideCards
    | ClearVotes
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
    | NoOp


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


type BackendMsg
    = UserConnected Lamdera.SessionId Lamdera.ClientId
    | UserDisconnected Lamdera.SessionId Lamdera.ClientId


type ToFrontend
    = SendRoomIdToFE RoomParam
    | ResRoomRoute
        { status : Status
        , roomName : ValidTextField
        , sessionId : Lamdera.SessionId
        , users : List User
        , stories : List Story
        }
    | UpdateRoom
        { sessionId : Lamdera.SessionId
        , name : ValidTextField
        }
    | SupplyBEData
        { users : List User
        , stories : List Story
        }
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
