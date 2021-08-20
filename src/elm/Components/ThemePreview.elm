module Components.ThemePreview exposing (State, init, render)

import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E

import Theme exposing (Theme)
import RGB255 exposing (RGB255)

type PreviewMode
  = PreviewModeFg
  | PreviewModeBg

type alias State =
  { previewMode : PreviewMode
  }

init : State
init = { previewMode = PreviewModeBg }

render : Theme RGB255 -> Theme.Key -> State -> Html State
render theme currentKey ({ previewMode } as state) =
  let
    keys : List Theme.Key
    keys =
      (if state.previewMode == PreviewModeBg then Theme.Fg else Theme.Bg)
      :: Theme.normalKeys ++ Theme.brightKeys

  in
    H.aside
      [ A.class "theme-preview"
      ]
      [ H.ul
        [ A.class "theme-preview__color-grid" ]
        (keys |> List.map (renderColor theme currentKey previewMode))
      , H.div
        [ A.class "theme-preview__tools" ]
        [ H.button
          [ A.class "theme-preview__button"
          , E.onClick (State (toggleMode previewMode))
          ]
          [ H.text "bg/fg" ]
        ]
      ]

renderColor : Theme RGB255 -> Theme.Key -> PreviewMode -> Theme.Key -> Html msg
renderColor theme currentKey previewMode key =
  let
    (fgKey, bgKey) =
      if previewMode == PreviewModeBg then ( key, currentKey ) else ( currentKey, key )
  in
    H.li
    [ A.class "theme-preview__color"
    , A.classList
      [ ( "theme-preview__color--basic"
        , key == Theme.Fg || key == Theme.Bg
        )
      ]
    , A.style "color"
      ((Theme.get fgKey theme) |> RGB255.toHex)
    , A.style "background-color"
      ((Theme.get bgKey theme) |> RGB255.toHex)
    ]
    [ H.text (Theme.nameOf key) ]

toggleMode : PreviewMode -> PreviewMode
toggleMode previewMode =
  case previewMode of
    PreviewModeFg -> PreviewModeBg
    PreviewModeBg -> PreviewModeFg
