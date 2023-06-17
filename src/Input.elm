module Input exposing (..)

import Css
import Html.Styled as Html exposing (Attribute, Html)
import Html.Styled.Attributes as Attr
import Html.Styled.Events exposing (onInput)
import Tailwind.Breakpoints as Bp
import Tailwind.Theme as Tw
import Tailwind.Utilities as Tw
import Types exposing (..)
import Util



{-
   inputStyle
       |> withError model.error
       |> withSendOnEnter
           (Util.onEnterWithCred
               Admin
               SendName
           )
       |> withPlaceholder "eq: Steve"
       |> viewInput StoreName
           (model.name
               |> Maybe.withDefault ""
           )
   ]
-}


type Input constraints
    = Input
        { textValue : String
        , placeholderText : String
        , handleInput : String -> FrontendMsg
        , styles : List Css.Style
        , error : InvalidTextFiled
        , msgOnEnter : Attribute FrontendMsg
        }


new : Input { needsInteractivity : () }
new =
    Input
        { textValue = ""
        , placeholderText = ""
        , handleInput = NoOpWithText
        , styles = []
        , error = Nothing
        , msgOnEnter = Attr.css []
        }



{-
   Interactive input field
-}


withHandleInput : (String -> FrontendMsg) -> String -> Input { needsInteractivity : () } -> Input { hasInteractivity : () }
withHandleInput toMsg value (Input constraints) =
    Input { constraints | handleInput = toMsg, textValue = value }


withSendOnEnter : FrontendMsg -> Input { constrainsts | hasInteractivity : () } -> Input { constrainsts | hasInteractivity : () }
withSendOnEnter msg (Input constraints) =
    Input { constraints | msgOnEnter = Util.onEnter msg }


withPlaceholder : String -> Input { constrainsts | hasInteractivity : () } -> Input { constrainsts | hasInteractivity : () }
withPlaceholder str (Input constraints) =
    Input { constraints | placeholderText = str }


withPrimaryStyles : Input { constraints | hasInteractivity : () } -> Input { constraints | hasInteractivity : () }
withPrimaryStyles (Input constraints) =
    Input { constraints | styles = inputStyle }


withError : InvalidTextFiled -> Input { constraints | hasInteractivity : () } -> Input { constraints | hasError : (), hasInteractivity : () }
withError maybeError (Input constraints) =
    case maybeError of
        Just _ ->
            Input
                { constraints
                    | styles =
                        constraints.styles
                            ++ [ Tw.border_2
                               , Tw.border_color Tw.red_500
                               , Tw.border_solid
                               , Css.focus
                                    [ Tw.ring_color Tw.red_500
                                    , Tw.border_none
                                    ]
                               ]
                }

        Nothing ->
            Input constraints


toHtml : Input { constraints | hasError : (), hasInteractivity : () } -> Html FrontendMsg
toHtml (Input constraints) =
    let
        { textValue, placeholderText, handleInput, styles, msgOnEnter } =
            constraints
    in
    Html.input
        [ Attr.type_ "text"
        , Attr.css styles
        , onInput handleInput
        , Attr.value textValue
        , Attr.placeholder placeholderText
        , msgOnEnter
        ]
        []



{-
   Read-only Input field
-}


withReadOnlyStyle : String -> Input { constraints | needsInteractivity : () } -> Input { constraints | isReadOnly : () }
withReadOnlyStyle str (Input constraints) =
    Input { constraints | styles = constraints.styles ++ inputReadOnlyStyle, textValue = str }


withStaticValue : String -> Input { constraints | isReadOnly : () } -> Input { constraints | isReadOnly : () }
withStaticValue str (Input constraints) =
    Input { constraints | textValue = str }


toReadOnlyHtml : Input { constraints | isReadOnly : () } -> Html FrontendMsg
toReadOnlyHtml (Input constraints) =
    let
        { textValue, styles } =
            constraints
    in
    Html.input
        [ Attr.type_ "text"
        , Attr.readonly True
        , Attr.css styles
        , Attr.value textValue
        ]
        []



{-
   Edit Input field
-}


withEditStyle : Input { constraints | hasInteractivity : () } -> Input { constraints | hasInteractivity : () }
withEditStyle (Input constraints) =
    Input { constraints | styles = constraints.styles ++ inputEditStyle }



--------------------------------------------


viewInput : (String -> FrontendMsg) -> String -> List (Attribute FrontendMsg) -> Html FrontendMsg
viewInput toMsg value attrs =
    Html.input
        ([ Attr.type_ "text"
         , onInput toMsg
         , Attr.value value
         ]
            ++ attrs
        )
        []


inputStyle : List Css.Style
inputStyle =
    [ Tw.block
    , Tw.w_full
    , Tw.form_input
    , Tw.rounded_md
    , Tw.border_0
    , Tw.py_1_dot_5
    , Tw.h_10
    , Tw.text_color Tw.gray_900
    , Tw.shadow_sm
    , Tw.ring_1
    , Tw.ring_inset
    , Tw.placeholder_color Tw.gray_300
    , Tw.text_lg
    , Tw.ring_color Tw.gray_300
    , Css.focus
        [ Tw.ring_2
        , Tw.ring_inset
        , Tw.ring_color Tw.teal_400
        ]
    ]


inputReadOnlyStyle : List Css.Style
inputReadOnlyStyle =
    [ Tw.block
    , Tw.w_full
    , Tw.form_input
    , Tw.rounded_md
    , Tw.rounded_b_none
    , Tw.border_0
    , Tw.py_2
    , Tw.pl_3
    , Tw.pr_3
    , Tw.shadow_sm
    , Tw.ring_1
    , Tw.ring_inset
    , Tw.ring_color Tw.gray_300
    , Tw.bg_color Tw.slate_900
    , Tw.font_mono
    , Tw.text_color Tw.teal_400
    , Css.focus
        [ Tw.outline_0
        , Tw.ring_color Tw.slate_900
        ]
    , Bp.sm
        [ Tw.text_lg
        , Tw.leading_6
        ]
    , Bp.lg [ Tw.text_right ]
    ]


inputEditStyle : List Css.Style
inputEditStyle =
    [ Tw.block
    , Tw.form_input
    , Tw.rounded_sm
    , Tw.rounded_r_none
    , Tw.border_0
    , Tw.py_2
    , Tw.px_2
    , Tw.shadow_sm
    , Tw.ring_1
    , Tw.h_12
    , Tw.text_2xl
    , Tw.flex_1
    , Tw.font_light
    , Tw.ring_inset
    , Tw.ring_color Tw.gray_300
    , Tw.bg_color Tw.slate_900
    , Tw.text_color Tw.white
    , Css.focus
        [ Tw.outline_0
        , Tw.ring_0
        ]
    ]
