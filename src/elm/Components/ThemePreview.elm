module Components.ThemePreview exposing (State, init, render)

import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import List.Extra exposing (interweave)

import Theme exposing (Theme)
import RGB255 exposing (RGB255)
import List.Extra exposing (interweave)

type PreviewMode
  = PreviewModeFg
  | PreviewModeBg

type alias State =
  { previewMode : PreviewMode
  }

init : State
init = { previewMode = PreviewModeBg }

render : Theme RGB255 -> Theme.Key -> State -> Html State
render theme currentKey { previewMode } =
  let
    colorCell key =
      renderColorCell
        (Theme.get key theme)
        (Theme.get currentKey theme)
        (Theme.nameOf key)
        ( previewMode == PreviewModeFg )
  in
  H.aside
    [ A.class "theme-preview"
    ]
    [ H.ul
      [ A.class "theme-preview__color-grid"
      , A.class "theme-preview__color-grid--type_basic"
      ]
      [ (renderColorCell theme.basic.fg theme.basic.bg "normal" False)
      , (renderColorCell theme.basic.fg theme.basic.bg "invert" True)
      ]
    , H.ul
      [ A.class "theme-preview__color-grid"
      , A.class "theme-preview__color-grid--type_other" ]
      (interweave (Theme.normalKeys) (Theme.brightKeys) |> List.map colorCell)
    , H.button
      [ A.class "theme-preview__button"
      , E.onClick (State (toggleMode previewMode))
      ]
      [ H.text "fg/bg" ]

    ]

renderColorCell : RGB255 -> RGB255 -> String -> Bool -> Html msg
renderColorCell fg bg name swap =
  H.li
    [ A.class "theme-preview__color-cell"
    , A.style "color" (RGB255.toHex (if swap then bg else fg))
    , A.style "background-color" (RGB255.toHex (if swap then fg else bg))
    ]
    [ H.text name ]

toggleMode : PreviewMode -> PreviewMode
toggleMode previewMode =
  case previewMode of
    PreviewModeFg -> PreviewModeBg
    PreviewModeBg -> PreviewModeFg
