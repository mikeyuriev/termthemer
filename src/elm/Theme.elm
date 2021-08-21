module Theme exposing (..)

import Dict
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra


import RGB255 exposing (RGB255)

type alias Basic i =
  { bg : i
  , fg : i
  }

type alias Group i =
  { blk : i
  , red : i
  , grn : i
  , ylw : i
  , blu : i
  , mag : i
  , cyn : i
  , wht : i
  }

type alias Theme i =
  { basic : Basic i
  , normal : Group i
  , bright : Group i
  }

type Key
  = Bg
  | Fg
  | Blk
  | Red
  | Grn
  | Ylw
  | Blu
  | Mag
  | Cyn
  | Wht
  | BBlk
  | BRed
  | BGrn
  | BYlw
  | BBlu
  | BMag
  | BCyn
  | BWht

get : Key -> Theme i -> i
get key { basic, normal, bright } =
  case key of
    Bg -> basic.bg
    Fg -> basic.fg
    Blk -> normal.blk
    Red -> normal.red
    Grn -> normal.grn
    Ylw -> normal.ylw
    Blu -> normal.blu
    Mag -> normal.mag
    Cyn -> normal.cyn
    Wht -> normal.wht
    BBlk -> bright.blk
    BRed -> bright.red
    BGrn -> bright.grn
    BYlw -> bright.ylw
    BBlu -> bright.blu
    BMag -> bright.mag
    BCyn -> bright.cyn
    BWht -> bright.wht

set : Key -> i -> Theme i -> Theme i
set key item theme =
  let
    ( basic, normal, bright ) = ( theme.basic, theme.normal, theme.bright )
  in
    case key of
      Bg -> { theme | basic = { basic | bg = item } }
      Fg -> { theme | basic = { basic | fg = item } }
      Blk -> { theme | normal = { normal | blk = item } }
      Red -> { theme | normal = { normal | red = item } }
      Grn -> { theme | normal = { normal | grn = item } }
      Ylw -> { theme | normal = { normal | ylw = item } }
      Blu -> { theme | normal = { normal | blu = item } }
      Mag -> { theme | normal = { normal | mag = item } }
      Cyn -> { theme | normal = { normal | cyn = item } }
      Wht -> { theme | normal = { normal | wht = item } }
      BBlk -> { theme | bright = { bright | blk = item } }
      BRed -> { theme | bright = { bright | red = item } }
      BGrn -> { theme | bright = { bright | grn = item } }
      BYlw -> { theme | bright = { bright | ylw = item } }
      BBlu -> { theme | bright = { bright | blu = item } }
      BMag -> { theme | bright = { bright | mag = item } }
      BCyn -> { theme | bright = { bright | cyn = item } }
      BWht -> { theme | bright = { bright | wht = item } }

keyToStr : Key -> String
keyToStr key =
  case key of
    Bg -> "bg"
    Fg -> "fg"
    Blk -> "blk"
    Red -> "red"
    Grn -> "grn"
    Ylw -> "ylw"
    Blu -> "blu"
    Mag -> "mag"
    Cyn -> "cyn"
    Wht -> "wht"
    BBlk -> "bblk"
    BRed -> "bred"
    BGrn -> "bgrn"
    BYlw -> "bylw"
    BBlu -> "bblu"
    BMag -> "bmag"
    BCyn -> "bcyn"
    BWht -> "bwht"

keys : List Key
keys = basicKeys ++ normalKeys ++ brightKeys

basicKeys : List Key
basicKeys = [ Bg, Fg ]

normalKeys : List Key
normalKeys = [ Blk, Red, Grn, Ylw, Blu, Mag, Cyn, Wht ]

brightKeys : List Key
brightKeys = [ BBlk, BRed, BGrn, BYlw, BBlu, BMag, BCyn, BWht ]

basics : Theme i -> List i
basics { basic } = [ basic.bg, basic.fg ]

normals : Theme i -> List i
normals { normal } = groupItems normal

brights : Theme i -> List i
brights { bright } = groupItems bright

items : Theme i -> List i
items theme = basics theme ++ normals theme ++ brights theme

groupItems : Group i -> List i
groupItems { blk, red, grn, ylw, blu, mag, cyn, wht } =
    [ blk, red, grn, ylw, blu, mag, cyn, wht ]

nameOf : Key -> String
nameOf key =
  case key of
    Bg -> "bg"
    Fg -> "fg"
    Blk -> "black"
    Red -> "red"
    Grn -> "green"
    Ylw -> "yellow"
    Blu -> "blue"
    Mag -> "magenta"
    Cyn -> "cyan"
    Wht -> "white"
    BBlk -> "black+"
    BRed -> "red+"
    BGrn -> "green+"
    BYlw -> "yellow+"
    BBlu -> "blue+"
    BMag -> "magenta+"
    BCyn -> "cyan+"
    BWht -> "white+"

xrNameOf : Key -> String
xrNameOf key =
  case key of
    Bg -> "background"
    Fg -> "foreground"
    Blk -> "color0"
    Red -> "color1"
    Grn -> "color2"
    Ylw -> "color3"
    Blu -> "color4"
    Mag -> "color5"
    Cyn -> "color6"
    Wht -> "color7"
    BBlk -> "color8"
    BRed -> "color9"
    BGrn -> "color10"
    BYlw -> "color11"
    BBlu -> "color12"
    BMag -> "color13"
    BCyn -> "color14"
    BWht -> "color15"

keyByXrName : String -> Maybe Key
keyByXrName xrName = List.Extra.find (\key -> xrNameOf key == xrName) keys

xRDefaults : Theme RGB255
xRDefaults =
  { basic =
    { bg = RGB255 0 0 0
    , fg = RGB255 128 128 128
    }
  , normal =
    { blk = RGB255 0 0 0
    , red = RGB255 128 0 0
    , grn = RGB255 0 128 0
    , ylw = RGB255 128 128 0
    , blu = RGB255 0 0 128
    , mag = RGB255 128 0 128
    , cyn = RGB255 0 128 128
    , wht = RGB255 128 128 128
    }
  , bright =
    { blk = RGB255 64 64 64
    , red = RGB255 255 0 0
    , grn = RGB255 0 255 0
    , ylw = RGB255 255 255 0
    , blu = RGB255 0 0 255
    , mag = RGB255 255 0 255
    , cyn = RGB255 0 255 255
    , wht = RGB255 255 255 255
    }
  }

xResourcesOf : Theme RGB255 -> String
xResourcesOf theme =
  keys
  |> List.map (\key -> "*." ++ (xrNameOf key) ++ ": " ++ (get key theme |> RGB255.toHex))
  >> String.join "\n"

decoder : (Decode.Decoder i) -> Decode.Decoder (Theme i)
decoder itemDecoder =
  let
    basicDecoder : Decode.Decoder (Basic i)
    basicDecoder =
      Decode.map2 Basic
        (Decode.field "bg" itemDecoder)
        (Decode.field "fg" itemDecoder)

    groupDecoder : Decode.Decoder (Group i)
    groupDecoder =
      Decode.map8 Group
        (Decode.field "blk" itemDecoder)
        (Decode.field "red" itemDecoder)
        (Decode.field "grn" itemDecoder)
        (Decode.field "ylw" itemDecoder)
        (Decode.field "blu" itemDecoder)
        (Decode.field "mag" itemDecoder)
        (Decode.field "cyn" itemDecoder)
        (Decode.field "wht" itemDecoder)
  in
    Decode.map3 Theme
      (Decode.field "basic" basicDecoder)
      (Decode.field "normal" groupDecoder)
      (Decode.field "bright" groupDecoder)

encode : Theme i -> (i -> Encode.Value) -> Encode.Value
encode { basic, normal, bright } encodeItem =
  let
    encodeBasic : Basic i -> Encode.Value
    encodeBasic { bg, fg } =
      Encode.object
        [ ( "bg", encodeItem bg )
        , ( "fg", encodeItem fg )
                ]
    encodeGroup : Group i -> Encode.Value
    encodeGroup { blk, red, grn, ylw, blu, cyn, mag, wht } =
      Encode.object
        [ ( "blk", encodeItem blk )
        , ( "red", encodeItem red )
        , ( "grn", encodeItem grn )
        , ( "ylw", encodeItem ylw )
        , ( "blu", encodeItem blu )
        , ( "mag", encodeItem mag )
        , ( "cyn", encodeItem cyn )
        , ( "wht", encodeItem wht )
        ]
  in
    Encode.object
      [ ( "basic", encodeBasic basic )
      , ( "normal", encodeGroup normal )
      , ( "bright", encodeGroup bright )
      ]

