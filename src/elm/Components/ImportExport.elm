module Components.ImportExport exposing (State, init, render)

import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E

import RGB255 exposing (RGB255)
import Theme exposing (Theme)

type alias State =
  { content : String
  }

init : State
init = { content = "" }

render : Theme RGB255 -> State -> Html ( State, Maybe String )
render theme { content } =
  H.aside
    [ A.class "import-export"
    ]
    [ H.textarea
      [ A.class "import-export__content"
      , A.value content
      , A.rows 19
      , A.spellcheck False
      , E.onInput (\value -> ( State value, Nothing ))
      ]
      []
    , H.div
      [ A.class "import-export__buttons"
      ]
      [ H.button
        [ A.class "import-export__button import-export__button--fn_import"
        , A.type_ "button"
        , A.disabled (content |> String.isEmpty)
        , E.onClick ( State content, Just content )
        ]
        [ H.text "Import"
        ]
      , H.button
        [ A.class "import-export__button import-export__button--fn_export"
        , A.type_ "button"
        , E.onClick ( State (Theme.xResourcesOf theme), Nothing )
        ]
        [ H.text "Export"
        ]
      ]
    ]
