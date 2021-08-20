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
  let
    grad start end = (RGB255.toHex start) ++ " 0%, " ++ (RGB255.toHex end) ++ " 100%"
    grad_ start mid end =
      (RGB255.toHex start) ++ " 0%, "
      ++ (RGB255.toHex mid) ++ " 50%, "
      ++ (RGB255.toHex end) ++ " 100%"

    gradR = grad (RGB255.rgb 0.0 g b) (RGB255.rgb 1.0 g b)
    gradG = grad (RGB255.rgb r 0.0 b) (RGB255.rgb r 1.0 b)
    gradB = grad (RGB255.rgb r g 0.0) (RGB255.rgb r g 1.0)
    gradH =
      "#ff0000 0%, #ffff00 17%, #00ff00 33%, #00ffff 50%, #0000ff 67%, #ff00ff 83%, #ff0000 100%"
    gradS = grad (RGB255.hsl h 0.0 l) (RGB255.hsl h 1.0 l)
    gradL = grad_ (RGB255.hsl h s 0.0) (RGB255.hsl h s 0.5) (RGB255.hsl h s 1.0)
  in
  H.div
    [ A.class "color-editor__tools"
    , A.class "color-tools"
    ]
    [ renderControl "r" "red:" gradR r |> H.map (\n -> stateFromRGB n g b )
    , renderControl "g" "green:" gradG g |> H.map (\n -> stateFromRGB r n b)
    , renderControl "b" "blue:" gradB b |> H.map (\n -> stateFromRGB r g n)
    , renderControl "h" "hue:" gradH h |> H.map (\n -> stateFromHSL n s l)
    , renderControl "s" "saturation:" gradS s |> H.map (\n -> stateFromHSL h n l)
    , renderControl "l" "lighness:" gradL l |> H.map (\n -> stateFromHSL h s n)
    ]

renderControl : String -> String -> String -> Float -> Html Float
renderControl key labelText grad value =
  H.div
    [ A.class "color-tools__item"
    , A.class ("color-tools__item--param_" ++ key)
    , A.attribute "style" ("--gradient: linear-gradient(to right, " ++ grad ++ ");")
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
      , A.class ("color-tools__control--param_" ++ key)
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

