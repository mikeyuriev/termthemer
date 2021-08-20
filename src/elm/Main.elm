port module Main exposing (main)

import Browser
import Html as H exposing (Html)
import Html.Attributes as A
import Json.Decode as Decode
import Json.Encode as Encode
import Platform.Cmd exposing (Cmd)
import Platform.Sub exposing (Sub)

import Theme exposing (Theme)
import XrParser
import RGB255 exposing (RGB255)
import Components.ColorEditor as ColorEditor
import Components.ColorSelector as ColorSelector
import Components.GithubIcon as GithubIcon
import Components.ImportExport as ImportExport
import Components.ThemePreview as ThemePreview

port saveState : String -> Cmd msg

type alias Model =
  { theme : Theme RGB255
  , currentKey : Theme.Key
  , colorEditorState : ColorEditor.State
  , themePreviewState : ThemePreview.State
  , importExportState : ImportExport.State
  }

type Msg
  = OnCurrentKeyChange Theme.Key
  | OnColorChange RGB255
  | OnImport String
  | SaveState
  | ColorEditorMsg ( ColorEditor.State, RGB255 )
  | ThemePreviewMsg ThemePreview.State
  | ImportExportMsg ( ImportExport.State, Maybe String )

init : String -> ( Model, Cmd msg )
init state =
  let
    theme =
      Decode.decodeString (Theme.decoder RGB255.decoder) state
      |> Result.withDefault Theme.xRDefaults
  in
  ( { theme = theme
    , currentKey = Theme.Fg
    , colorEditorState = ColorEditor.init (Theme.get Theme.Fg theme)
    , themePreviewState = ThemePreview.init
    , importExportState = ImportExport.init
    }
  , Cmd.none
  )

view : Model -> Html Msg
view
  { theme
  , currentKey
  , colorEditorState
  , themePreviewState
  , importExportState
  } =
  H.div
    [ A.class "app"
    ]
    [ H.header
      [ A.class "app__header"
      ]
      [ H.h1
        [ A.class "app__title"
        ]
        [ H.text "TermThemer"
        ]
      , H.a
        [ A.class "app__link"
        , A.href "https://github.com/mikeyuriev/termthemer"
        , A.target "_blank"
        ]
        [ GithubIcon.render
        ]
      ]
    , ColorSelector.render theme currentKey |> H.map OnCurrentKeyChange
    , ColorEditor.render
        (Theme.nameOf currentKey)
        (Theme.xrNameOf currentKey)
        colorEditorState
        |> H.map ColorEditorMsg
    , ThemePreview.render
        theme
        currentKey
        themePreviewState
        |> H.map ThemePreviewMsg
    , ImportExport.render theme importExportState |> H.map ImportExportMsg
    ]

update : Msg -> Model -> ( Model, Cmd msg )
update msg ({ theme, currentKey } as model) =
  case msg of
    OnCurrentKeyChange key ->
      ( { model
        | currentKey = key
        , colorEditorState = ColorEditor.init (Theme.get key theme)
        }
      , Cmd.none
      )
    OnColorChange color ->
      { model | theme = Theme.set currentKey color theme }
      |> update SaveState
    OnImport value ->
      { model | theme = XrParser.parse value }
      |> update SaveState

    SaveState ->
      ( model
      , saveState (Theme.encode theme RGB255.encode |> Encode.encode 0)
      )

    ColorEditorMsg ( state, color ) ->
      { model| colorEditorState = state }
      |> update (OnColorChange color)
    ThemePreviewMsg state ->
      ( { model | themePreviewState = state } , Cmd.none )
    ImportExportMsg ( state, value ) ->
      { model | importExportState = state }
      |> (\model_ ->
        case value of
          Nothing -> ( model_, Cmd.none )
          Just value_ -> update (OnImport value_) model_
      )

main : Program String Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = (\_ -> Sub.none)
    }
