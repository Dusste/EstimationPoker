module Evergreen.V18.Types exposing (..)

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


type EditRoomName
    = WrappedEditState
    | UnwrappedEditState


type alias StoryId =
    Int


type ValidTextField
    = ValidTextField String


type alias StoryName =
    ValidTextField


type Story
    = Story StoryId StoryName
    | Edit StoryId StoryName
    | NoStory


type alias InvalidTextFiled =
    Maybe String


type alias RoomParam =
    Int


type alias Card =
    Float


type VoteState
    = NotVoted
    | HiddenVote Card
    | Voted Card


type alias User =
    { name : ValidTextField
    , isAdmin : Bool
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
    , name : String
    , roomName : String
    , editedRoomName : EditRoomName
    , story : Story
    , stories : List Story
    , error : InvalidTextFiled
    , roomId : Maybe RoomParam
    , users : List User
    , sessionId : Maybe Lamdera.SessionId
    , clock : Int
    , shouldEnableCustomSequence : Bool
    , storyCount : Int
    , sequenceAsInput : String
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
    | StoreEditStory String
    | SendName Credentials
    | SendRoom
    | SendEditedRoom
    | SendStory
    | SendEditedStory Int
    | SaveStory
    | ChooseCard Card String
    | Tick Time.Posix
    | StartTime
    | ResetTime
    | ShowCards
    | HideCards
    | ClearVotes
    | StoreSequence SequenceString
    | CheckSequence
    | SendSequence
    | SelectSequence CommonSequence
    | FinishVoting
    | NextStory
    | SkipStory
    | StartChartAnimation
    | ShowChart Chart
    | HideNotification
    | CopyRoomUrl
    | EditStory StoryId StoryName
    | EditRoomName
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
    | SendCard Card RoomParam Lamdera.SessionId
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


type alias RoomResponse =
    { status : Status
    , roomName : ValidTextField
    , sessionId : Lamdera.SessionId
    , users : List User
    , stories : List Story
    }


type ToFrontend
    = SendRoomIdToFE RoomParam
    | ResRoomRoute RoomResponse
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
