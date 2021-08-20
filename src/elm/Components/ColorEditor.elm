module Components.ColorEditor exposing (State, init, render)

import Json.Decode as Decode
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E

import RGB255 exposing (RGB255)

type alias State =
  { input : String
  , r : Float
  , g : Float
  , b : Float
  , h : Float
  , s : Float
  , l : Float
  }

init : RGB255 -> State
init color =
  let
    { r, g, b } = RGB255.toRGB color
    { h, s, l } = RGB255.toHSL color
  in
    State (RGB255.toHex color) r g b h s l

render : String -> String -> State -> Html ( State, RGB255 )
render name xrName state =
  let
    toMsg state_ = ( state_, (rgb255OfState state_) )
  in
  H.main_
    [ A.class "color-editor"
    ]
    [ renderSummary name xrName state |> H.map toMsg
    , renderTools state |> H.map toMsg
    ]

renderHexInput : State -> Html State
renderHexInput state =
  H.input
    [ A.class "color-summary__value"
    , A.value (hexOfState state)
    , E.on "change" (E.targetValue |> Decode.map (stateFromHex state))
    ]
    []

renderSummary : String -> String -> State -> Html State
renderSummary name xrName state =
  H.div
    [ A.class "color-editor__summary color-summary"
    ]
    [ H.div
      [ A.class "color-summary__box"
      , A.style "background-color" (hexOfState state)
      ]
      []
    , H.div
      [ A.class "color-summary__info"
      ]
      [ H.div
        [ A.class "color-summary__text color-summary__text--content_name"
        ]
        [ H.text name
        ]
      , H.div
        [ A.class "color-summary__text color-summary__text--content_xr-name"
        ]
        [ H.text xrName
        ]
      ]
    , renderHexInput state
    ]

renderTools : State -> Html State
renderTools { r, g, b, h, s, l } =
  H.div
    [ A.class "color-editor__tools"
    , A.class "color-tools"
    ]
    [ renderControl "r" "red:" r |> H.map (\n -> stateFromRGB n g b )
    , renderControl "g" "green:" g |> H.map (\n -> stateFromRGB r n b)
    , renderControl "b" "blue:" b |> H.map (\n -> stateFromRGB r g n)
    , renderControl "h" "hue:" h |> H.map (\n -> stateFromHSL n s l)
    , renderControl "s" "saturation:" s |> H.map (\n -> stateFromHSL h n l)
    , renderControl "l" "lighness:" l |> H.map (\n -> stateFromHSL h s n)
    ]

renderControl : String -> String -> Float -> Html Float
renderControl key labelText value =
  H.div
    [ A.class "color-tools__item"
    , A.class ("color-tools__item--param_" ++ key)
    ]
    [ H.label
      [ A.class "color-tools__label"
      , A.for ("COLOR-CONTROL-" ++ String.toUpper key)
      ]
      [ H.text labelText
      ]
    , H.input
      [ A.id ("COLOR-CONTROL-" ++ String.toUpper key)
      , A.class "color-tools__control"
      , A.type_ "range"
      , A.max "1"
      , A.step "any"
      , A.value (String.fromFloat value)
      , E.onInput (String.toFloat >> Maybe.withDefault 0.0)
      ]
      []
    ]

rgb255OfState : State -> RGB255
rgb255OfState { r, g, b } = RGB255.RGB r g b |> RGB255.fromRGB

hexOfState : State -> String
hexOfState = rgb255OfState >> RGB255.toHex

stateFromRGB : Float -> Float -> Float -> State
stateFromRGB r g b =
  let
    rgb = RGB255.RGB r g b
    color = RGB255.fromRGB rgb
    { h, s, l } = RGB255.toHSL color
  in
    State (RGB255.toHex color) r g b h s l

stateFromHSL : Float -> Float -> Float -> State
stateFromHSL h s l =
  let
    hsl = RGB255.HSL h s l
    color = RGB255.fromHSL hsl
    { r, g, b } = RGB255.toRGB color
  in
    State (RGB255.toHex color) r g b h s l

stateFromHex : State -> String -> State
stateFromHex fallback value =
  case RGB255.fromHex (String.trim value) of
    Nothing -> fallback
    Just color ->
      let
        { h, s, l } = RGB255.toHSL color
        { r, g, b } = RGB255.toRGB color
      in
        State (RGB255.toHex color) r g b h s l

