module Components.ColorSelector exposing (render)

import Html as H exposing (Html, Attribute)
import Html.Attributes as A
import Html.Events as E
import List.Extra

import Theme exposing (Theme)
import RGB255 exposing (RGB255)

render : Theme RGB255 -> Theme.Key -> Html Theme.Key
render theme currentKey =
  H.aside
    [ A.class "color-selector"
    ]
    [ H.ul
      [ A.class "color-selector__content"
      ]
      ( Theme.basicKeys
        ++ (List.Extra.interweave Theme.normalKeys Theme.brightKeys)
        |> List.map (renderButton theme currentKey)
      )
    ]

renderButton : Theme RGB255 -> Theme.Key -> Theme.Key -> Html Theme.Key
renderButton theme currentKey key =
  let
    color = Theme.get key theme

    textMod : Attribute msg
    textMod =
      A.class
        ( "color-selector__button--text_"
          ++ if RGB255.isLight color then "dark" else "light"
        )
  in
    H.li
      [ A.class "color-selector__color"
      , A.classList
        [ ( "color-selector__color--current", key == currentKey )
        ]
      ]
      [ H.button
        [ A.type_ "button"
        , A.class "color-selector__button"
        , textMod
        , A.style "background-color" (color |> RGB255.toHex)
        , E.onClick key
        ]
        [ H.text (Theme.nameOf key)
        ]
      ]

