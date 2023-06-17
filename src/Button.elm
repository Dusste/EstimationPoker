module Button exposing (..)

import Css
import Html.Styled as Html exposing (Html, text)
import Html.Styled.Attributes as Attr
import Html.Styled.Events exposing (onClick)
import Tailwind.Theme as Tw
import Tailwind.Utilities as Tw
import Types exposing (..)



-- type Not_DisabledOrClickable
--     = Not_DisabledOrClickable Never
-- type DisabledOrClickable
--     = DisabledOrClickable Never
-- type NoTextOrIcon
--     = NoTextOrIcon Never
-- type HasTextOrIcon
--     = HasTextOrIcon Never
-- new : Button Not_DisabledOrClickable msg
-- new =
--     Button { text = "" }
-- new : Button NoTextOrIcon msg
-- new =
--     Button
-- new : Button { needsInteractivity : () } msg
-- new :
{-
   Button.new
       -- Button { constraints | needsInteractivity : () } FrontendMsg
       |> Button.withText "Save"
       -- Button { constraints | hasTextOrIcon : () } FrontendMsg
       |> Button.withPrimaryStyle
       -- Button { constraints | hasTextOrIcon : () } FrontendMsg
       |> Button.withOnClick (SendName Admin)
       -- Button { hasInteractivity : () } FrontendMsg
       |> Button.toHtml
       -- Html FrontendMsg
-}


type Button constraints
    = Button { txt : String, handleClick : FrontendMsg, styles : List Css.Style, disabled : Bool }


new : Button { needsInteractivity : () }
new =
    Button { txt = "", handleClick = NoOp, styles = [], disabled = False }


withDisabled : Button { constraints | needsInteractivity : () } -> Button { constraints | hasInteractivity : () }
withDisabled (Button constraints) =
    Button { constraints | disabled = True }


withText : String -> Button { needsInteractivity : () } -> Button { hasTextOrIcon : () }
withText str (Button constraints) =
    Button { constraints | txt = str }


withPrimaryStyle : Button { constraints | hasTextOrIcon : () } -> Button { constraints | hasStyles : (), hasTextOrIcon : () }
withPrimaryStyle (Button constraints) =
    Button { constraints | styles = buttonStyle }


withEditStyle : Button { constraints | hasTextOrIcon : () } -> Button { constraints | hasStyles : (), hasTextOrIcon : () }
withEditStyle (Button constraints) =
    Button { constraints | styles = buttonEditStyle }


withReadOnlyInputStyle : Button { constraints | hasTextOrIcon : () } -> Button { constraints | hasStyles : (), hasTextOrIcon : () }
withReadOnlyInputStyle (Button constraints) =
    Button { constraints | styles = buttonStyle ++ [ Tw.rounded_t_none ] }


withOnClick : FrontendMsg -> Button { constraints | hasTextOrIcon : (), hasStyles : () } -> Button { constraints | hasInteractivity : (), hasTextOrIcon : (), hasStyles : () }
withOnClick message (Button constraints) =
    Button { constraints | handleClick = message }


toHtml : Button { constraints | hasInteractivity : (), hasTextOrIcon : (), hasStyles : () } -> Html FrontendMsg
toHtml (Button constraints) =
    let
        { txt, handleClick, styles } =
            constraints
    in
    Html.button [ onClick handleClick, Attr.css styles ] [ text txt ]


buttonStyle : List Css.Style
buttonStyle =
    [ Tw.bg_color Tw.teal_400
    , Tw.text_color Tw.white
    , Tw.py_1
    , Tw.px_4
    , Tw.text_xl
    , Tw.border
    , Tw.border_color Tw.teal_400
    , Tw.rounded
    , Tw.cursor_pointer
    , Tw.transition_all
    , Css.hover
        [ Tw.bg_color Tw.teal_700
        , Tw.border_color Tw.teal_400
        , Tw.border_color Tw.transparent
        ]
    ]


buttonEditStyle : List Css.Style
buttonEditStyle =
    [ Tw.bg_color Tw.teal_400
    , Tw.text_color Tw.white
    , Tw.py_2
    , Tw.px_2
    , Tw.text_xl
    , Tw.border
    , Tw.border_color Tw.teal_400
    , Tw.rounded_sm
    , Tw.rounded_l_none
    , Tw.cursor_pointer
    , Tw.transition_all
    , Css.hover
        [ Tw.bg_color Tw.teal_700
        , Tw.border_color Tw.teal_400
        , Tw.border_color Tw.transparent
        ]
    ]



-- withText : Button constraints msg -> Button HasTextOrIcon msg
-- withIcon : Button constraints msg -> Button HasTextOrIcon msg
-- toHtml : Button HasTextOrIcon msg -> Html msg
-- withOnClick : FrontendMsg -> Button FrontendMsg -> Button FrontendMsg
-- withOnClick onClick (Button btn) =
--     Button { btn | onClick = onClick }
-- withDisabled : Button Not_DisabledOrClickable msg -> Button DisabledOrClickable msg
-- withDisabled (Button btn) =
--     Button { btn | disabled = True }
-- withOnClick : msg -> Button Not_DisabledOrClickable msg -> Button DisabledOrClickable
-- withOnClick onClick (Button btn) =
--     Button { btn | onClick = onClick }
