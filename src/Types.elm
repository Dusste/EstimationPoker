module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Lamdera exposing (ClientId)
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , status : Status
    , name : Maybe String
    , roomName : Maybe String
    , stories : List String
    }


type Status
    = EnterNameStep
    | CreateRoomStep
    | CreateStoryStep
    | PokerStep


type alias BackendModel =
    { admin : Maybe String
    , users : Dict ClientId String
    , roomName : Maybe String
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | StoreName String
    | StoreRoom String
    | StoreStory String
    | SendName
    | SendRoom
    | SendStory


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
