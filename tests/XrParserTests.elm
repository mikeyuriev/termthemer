module XrParserTests exposing (..)

import Parser exposing (parse)
import Parser.Sequence exposing (exactly)
import Test exposing (..)
import Expect

import RGB255 exposing (RGB255)
import Theme
import XrParser as P

suite : Test
suite = describe "XrParser"
  [ describe "hexDigit"
    [ test "must parse correct input" <|
        \_ ->
          parse "0123456789abcdefABCDEF" (exactly 22 P.hexDigit)
          |> Expect.equal (Result.Ok (String.toList "0123456789abcdefABCDEF"))
    ]
  , describe "rgbColor"
    [ test "must parse rgb colors" <|
        \_ ->
          parse "rgb:3/a/c1" P.rgbColor
          |> Expect.equal (Result.Ok (RGB255 3 10 193))
    ]
  , describe "hexColor"
    [ test "must parse six-digits colors" <|
        \_ ->
          parse "#123456" P.hexColor
          |> Expect.equal (Result.Ok (RGB255 18 52 86))
    , test "must parse three-digits colors" <|
        \_ ->
          parse "#abc" P.hexColor
          |> Expect.equal (Result.Ok (RGB255 170 187 204))
    ]
  , describe "xrColorName"
    [ test "must parse color names" <|
        \_ ->
          let
            names = List.map Theme.xrNameOf Theme.keys
            oks = List.map Result.Ok Theme.keys
            parse_ parser s = parse s parser
          in
            List.map (parse_ P.xrColorName) names
            |> Expect.equalLists oks
    ]
  , describe "xrName"
    [ test "must parse valid xr name" <|
        \_ ->
          parse "_abCD_123" P.xrName
          |> Expect.equal (Result.Ok "_abCD_123")
    , test "must fail on invalid xr name" <|
        \_ ->
          parse "123abc" P.xrName
          |> Expect.err
    ]
  , describe "defineLn"
    [ test "must parse color #define" <|
        \_ ->
          parse "#define const #123456" P.defineLn
          |> Expect.equal (Result.Ok (P.Define "const" (P.Value (RGB255 18 52 86))))
    , test "must parse ref define" <|
        \_ ->
          parse "#define const1 const2" P.defineLn
          |> Expect.equal (Result.Ok (P.Define "const1" (P.Name "const2")))
    , test "must skip comments" <|
        \_ ->
          parse "#define const rgb:12/34/56 !comment" P.defineLn
          |> Expect.equal (Result.Ok (P.Define "const" (P.Value (RGB255 18 52 86))))
    , test "must skip spaces" <|
        \_ ->
          parse "  #define   const1   #123  !  comment  " P.defineLn
          |> Expect.equal (Result.Ok (P.Define "const1" (P.Value (RGB255 17 34 51))))
    , test "must fail on incomplete input" <|
        \_ ->
          parse "#define #1234" P.defineLn
          |> Expect.err
    ]
  , describe "colorLn"
    [ test "must parse color" <|
        \_ ->
          parse "*.color1: #123456" P.colorLn
          |> Expect.equal (Result.Ok (P.Color Theme.Red (P.Value (RGB255 18 52 86))))
    , test "must parse ref" <|
        \_ ->
          parse "*.color2: const" P.colorLn
          |> Expect.equal (Result.Ok (P.Color Theme.Grn (P.Name "const")))
    , test "must skip comments" <|
        \_ ->
          parse "*.color3: rgb:12/34/56 !comment" P.colorLn
          |> Expect.equal (Result.Ok (P.Color Theme.Ylw (P.Value (RGB255 18 52 86))))
    , test "must skip spaces" <|
        \_ ->
          parse "  *.color4:   #123  !  comment  " P.colorLn
          |> Expect.equal (Result.Ok (P.Color Theme.Blu (P.Value (RGB255 17 34 51))))
    , test "must fail on incomplete input" <|
        \_ ->
          parse "*.color5: #1234" P.colorLn
          |> Expect.err
    ]
  , describe "ln"
    [ test "must parse colors" <|
        \_ ->
          parse "*foreground:#123456" P.ln
          |> Expect.equal (Result.Ok (P.Color Theme.Fg (P.Value (RGB255 18 52 86))))
    , test "must parse #define" <|
      \_ ->
        parse " #define  abc  rgb:12/34/56   \n" P.ln
        |> Expect.equal (Result.Ok (P.Define "abc" (P.Value (RGB255 18 52 86))))
    , test "must ignore other lines" <|
        \_ ->
          parse "Line" P.ln
          |> Expect.equal (Result.Ok P.Ignore)
    ]
  ]

