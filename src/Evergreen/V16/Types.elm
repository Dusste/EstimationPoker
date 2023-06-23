module Evergreen.V16.Types exposing (..)

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
    = Intro
    | EnterNameStep
    | EnterAdminNameStep
    | CreateRoomStep
    | CreateStoryStep
    | StoryPointsSequenceStep
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


type alias SequenceString =
    String


type Sequence
    = Accept SequenceString
    | InTransition String
    | Reject


type CommonSequence
    = Default
    | Option2
    | Option3
    | Option4
    | CustomSequence SequenceString


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
    , shouldEnableCustomSequence : Bool
    , storyCount : Int
    , editedStory : Story
    , sequence : Sequence
    , chooseSequence : CommonSequence
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
    , sequence : CommonSequence
    }


type alias BackendModel =
    { rooms : Dict.Dict Index Room
    , index : Index
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | StartAsAdmin
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
    | EnableSequenceInput
    | SendCustomSequence
    | NoOp
    | NoOpWithText String


type ToBackend
    = SendStoryToBE (List Story) RoomParam
    | SendRoomNameToBE ValidTextField RoomParam
    | SendAdminNameToBE ValidTextField
    | SendUserNameToBE ValidTextField RoomParam
    | ReqRoomRoute RoomParam Credentials
    | SendCard Float RoomParam Lamdera.SessionId
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
    | SendCustomSequenceToBE SequenceString RoomParam


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
    | UpdateCards (List User) Lamdera.SessionId
    | UsersFlipCards (List User)
    | UsersCardReset (List User)
    | SkipStoryAndExposeCharts (List User)
    | ExposeCharts
    | UpdateStoriesAfterSkip (List Story) (List User)
    | UpdateStories (List Story)
    | ChartAnimation
    | UpdateRoomName ValidTextField
    | UpdateSequence CommonSequence
