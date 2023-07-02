module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Css exposing (Style)
import Dict exposing (Dict)
import Lamdera exposing (ClientId, SessionId)
import Time
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
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
    , sessionId : Maybe SessionId
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


type EditRoomName
    = WrappedEditState
    | UnwrappedEditState


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
    | Edit StoryId StoryName
    | NoStory


type alias StoryId =
    Int


type alias StoryName =
    ValidTextField


type CommonSequence
    = Default
    | Option2
    | Option3
    | Option4
    | CustomSequence SequenceString


type Sequence
    = Accept SequenceString
    | InTransition String
    | Reject


type alias SequenceConfig =
    { msg : CommonSequence -> FrontendMsg
    , choosenSequence : CommonSequence
    , sequenceValue : SequenceString
    , msgAttribute : CommonSequence
    , borderSetup : List Style
    , textValue : String
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


type ValidTextField
    = ValidTextField String


type Status
    = Intro
    | EnterNameStep
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


defaultUser : User
defaultUser =
    { name = ValidTextField "Name not available"
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
    | ChooseCard Float String
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


type Chart
    = Donut
    | Bar


type ToBackend
    = SendStoryToBE (List Story) RoomParam
    | SendRoomNameToBE ValidTextField RoomParam
    | SendAdminNameToBE ValidTextField
    | SendUserNameToBE ValidTextField RoomParam
    | ReqRoomRoute RoomParam Credentials
    | SendCard Float RoomParam SessionId
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
    = UserConnected SessionId ClientId
    | UserDisconnected SessionId ClientId


type alias RoomResponse =
    { status : Status, roomName : ValidTextField, sessionId : SessionId, users : List User, stories : List Story }


type ToFrontend
    = SendRoomIdToFE RoomParam
    | ResRoomRoute RoomResponse
    | UpdateRoom { sessionId : SessionId, name : ValidTextField }
    | SupplyBEData { users : List User, stories : List Story }
    | UsersStartTimer
    | UsersResetTimer
    | UpdateCards (List User) SessionId
    | UsersFlipCards (List User)
    | UsersCardReset (List User)
    | SkipStoryAndExposeCharts (List User)
    | ExposeCharts
    | UpdateStoriesAfterSkip (List Story) (List User)
    | UpdateStories (List Story)
    | ChartAnimation
    | UpdateRoomName ValidTextField
    | UpdateSequence CommonSequence
