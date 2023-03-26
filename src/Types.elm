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
    , clientId : Maybe ClientId
    , clock : Int
    , shouldStartClock : Bool
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
    { admin : ValidTextField
    , users : List User
    , roomName : ValidTextField
    , stories : List ValidTextField
    }


type alias User =
    { name : ValidTextField
    , isAdmin : Bool
    , card : Float
    , clientId : ClientId
    }


defaultRoom : Room
defaultRoom =
    { admin = ""
    , users = []
    , roomName = ""
    , stories = []
    }


defaultUser : User
defaultUser =
    { name = ""
    , isAdmin = False
    , card = 0
    , clientId = "123"
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
    | ChooseCard Float
    | Tick Time.Posix
    | StartTime
    | ResetTime


type ToBackend
    = SendStoryToBE (List ValidTextField) Int
    | SendRoomNameToBE ValidTextField Int
    | SendAdminNameToBE ValidTextField
    | SendUserNameToBE ValidTextField Int
    | ReqRoomRoute RoomParam Bool


type BackendMsg
    = UserConnected SessionId ClientId
    | UserDisconnected SessionId ClientId


type ToFrontend
    = SendRoomIdToFE Int
    | ResRoomRoute { status : Status, roomName : String, clientId : ClientId, users : List User, stories : List ValidTextField }
    | UpdateRoom { clientId : ClientId, name : String }
    | SupplyBEData { users : List User, stories : List ValidTextField }
    | UpdateUsers (List User)
