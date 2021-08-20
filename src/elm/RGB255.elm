module RGB255 exposing (..)

import Color exposing (Color)
import Color.Convert
import Json.Decode as Decode
import Json.Encode as Encode

type alias RGB255 =
  { r : Int
  , g : Int
  , b : Int
  }

type alias RGB =
  { r : Float
  , g : Float
  , b : Float
  }

type alias HSL =
  { h : Float
  , s : Float
  , l : Float
  }

rgb : Float -> Float -> Float -> RGB255
rgb r g b =
  let
    c = (*) 255 >> round
  in
    RGB255 (c r) (c g) (c b)

hsl : Float -> Float -> Float -> RGB255
hsl h s l = fromHSL (HSL h s l)

update : RGB255 -> Maybe Int -> Maybe Int -> Maybe Int -> RGB255
update { r, g, b } r_ g_ b_ =
  { r = Maybe.withDefault r r_
  , g = Maybe.withDefault g g_
  , b = Maybe.withDefault b b_
  }

updateRGB : RGB255 -> Maybe Float -> Maybe Float -> Maybe Float -> RGB255
updateRGB { r, g, b } r_ g_ b_ =
  let
    c = toFloat >> (/) 255
  in
    rgb
      (Maybe.withDefault (c r) r_)
      (Maybe.withDefault (c g) g_)
      (Maybe.withDefault (c b) b_)

updateHSL : RGB255 -> Maybe Float -> Maybe Float -> Maybe Float -> RGB255
updateHSL color h s l =
  toHSL color
  |> (\c ->
    { h = Maybe.withDefault c.h h
    , s = Maybe.withDefault c.s s
    , l = Maybe.withDefault c.l l
    }
  )
  |> fromHSL

toColor : RGB255 -> Color
toColor {r, g, b} = Color.rgb255 r g b

fromColor : Color -> RGB255
fromColor color =
  let
    fromInt_ = (*) 255 >> round
    { red, green, blue } = Color.toRgba color
    ( r, g, b ) = (fromInt_ red, fromInt_ green, fromInt_ blue)
  in
    RGB255 r g b

toHex : RGB255 -> String
toHex = toColor >> Color.Convert.colorToHex

fromHex : String -> Maybe RGB255
fromHex = Color.Convert.hexToColor >> Result.toMaybe >> Maybe.map fromColor

fromHex_ : RGB255 -> String -> RGB255
fromHex_ fallback = fromHex >> Maybe.withDefault fallback

toRGB : RGB255 -> RGB
toRGB { r, g, b } =
  let
    c = toFloat >> (\n -> n / 255)
  in
    RGB (c r) (c g) (c b)

fromRGB : RGB -> RGB255
fromRGB { r, g, b } =
  let
    c = (*) 255 >> round
  in
    RGB255 (c r) (c g) (c b)

toHSL : RGB255 -> HSL
toHSL =
  toColor
  >> Color.toHsla
  >> (\r -> HSL r.hue r.saturation r.lightness)

fromHSL : HSL -> RGB255
fromHSL { h, s, l } =
  Color.fromHsla { hue = h, saturation = s, lightness = l, alpha = 1.0 }
  |> fromColor

-- https://stackoverflow.com/a/3943023
isLight : RGB255 -> Bool
isLight { r, g, b } =
  let
    ( r_, g_, b_ ) = (toFloat r, toFloat g, toFloat b)
  in
    (r_ * 0.299 + g_ * 0.587 + b_ * 0.114) > 182

decoder : Decode.Decoder RGB255
decoder =
  Decode.map3 RGB255
    (Decode.field "r" Decode.int)
    (Decode.field "g" Decode.int)
    (Decode.field "b" Decode.int)

encode : RGB255 -> Encode.Value
encode { r, g, b } =
  Encode.object
    [ ( "r", Encode.int r )
    , ( "g", Encode.int g )
    , ( "b", Encode.int b )
    ]
