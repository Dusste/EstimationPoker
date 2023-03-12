module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Css
import Css.Global
import Html.Styled as Html exposing (Html, text)
import Html.Styled.Attributes as Attr
import Html.Styled.Events exposing (onClick, onInput)
import Lamdera
import Tailwind.Breakpoints as Breakpoints
import Tailwind.Theme as Tw
import Tailwind.Utilities as Tw
import Types exposing (..)
import Url


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \m -> Sub.none
        , view = view
        }


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    ( { key = key
      , status = EnterNameStep
      , name = Nothing
      , roomName = Nothing
      , stories = []
      }
    , Cmd.none
    )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            ( model, Cmd.none )

        SendName ->
            ( { model | status = CreateRoomStep }, Cmd.none )

        SendRoom ->
            ( { model | status = CreateStoryStep }, Cmd.none )

        SendStory ->
            ( { model | status = PokerStep }, Cmd.none )

        StoreName str ->
            ( { model | name = Just str }, Cmd.none )

        StoreRoom str ->
            ( { model | roomName = Just str }, Cmd.none )

        StoreStory str ->
            ( { model | stories = model.stories ++ [ str ] }, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )


view : Model -> Browser.Document FrontendMsg
view model =
    { title = ""
    , body =
        [ Html.toUnstyled <|
            Html.div []
                [ case model.status of
                    EnterNameStep ->
                        Html.div []
                            [ Html.div [] [ Html.h3 [] [ text "Add you name" ] ]
                            , Html.div []
                                [ Html.input [ onInput StoreName ] []
                                ]
                            , Html.div []
                                [ Html.button [ onClick SendName ] [ text "Save" ]
                                ]
                            ]

                    CreateRoomStep ->
                        Html.div []
                            [ Html.div [] [ Html.h3 [] [ text "Create new room" ] ]
                            , Html.div []
                                [ Html.input [ onInput StoreRoom ] []
                                ]
                            , Html.div []
                                [ Html.button [ onClick SendRoom ] [ text "Create" ]
                                ]
                            ]

                    CreateStoryStep ->
                        Html.div []
                            [ Html.div [] [ Html.h3 [] [ text "Create new story" ] ]
                            , Html.div []
                                [ Html.textarea [ onInput StoreStory ] []
                                ]
                            , Html.div []
                                [ Html.button [] [ text "Save and Add new" ]
                                , Html.button [ onClick SendStory ] [ text "Save and Close" ]
                                ]
                            ]

                    PokerStep ->
                        Html.div []
                            [ Html.div [] [ Html.h3 [] [ text "{ Room name that you added }" ] ]
                            , Html.div []
                                [ text "I am main content"
                                , Html.div [] [ text "I am card" ]
                                ]
                            , Html.div []
                                [ text "I am sidebar" ]
                            , Html.div []
                                [ text "I am bottom tabs" ]
                            ]
                ]
        ]
    }
