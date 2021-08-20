module XrParser exposing (..)

import Dict exposing (Dict)
import Hex
import Parser exposing (..)
import Parser.Char exposing (..)
import Parser.Check exposing (..)
import Parser.Common exposing (..)
import Parser.Sequence exposing (..)

import RGB255 exposing (RGB255)
import Theme exposing (Theme)

type Value
  = Value RGB255
  | Name String

type alias Symbols = Dict String RGB255

type Line
  = Color Theme.Key Value
  | Define String Value
  | Ignore

parse : String -> Theme RGB255
parse lines =
  let
    updateTheme : Theme RGB255 -> Symbols -> Theme.Key -> Value -> Theme RGB255
    updateTheme theme symbols key value =
      case value of
        Value color -> Theme.set key color theme
        Name name ->
          Dict.get name symbols
          |> Maybe.map (\color -> Theme.set key color theme)
          |> Maybe.withDefault theme

    updateSymbols : Symbols -> String -> Value -> Symbols
    updateSymbols symbols name value =
      case value of
        Value color -> Dict.insert name color symbols
        Name name_ ->
          Dict.get name_ symbols
          |> Maybe.map (\color -> Dict.insert name_ color symbols)
          |> Maybe.withDefault symbols

    update : ( Theme RGB255, Symbols ) -> Line -> ( Theme RGB255, Symbols )
    update ( theme, symbols ) line =
      case line of
        Color key value -> ( updateTheme theme symbols key value, symbols )
        Define name value -> ( theme, updateSymbols symbols name value )
        Ignore -> ( theme, symbols )
  in
    Parser.parse
      lines
      (fold update ( Theme.xRDefaults, Dict.empty ) ln |> map (\(a, _) -> a))
    |> Result.withDefault Theme.xRDefaults

ln : Parser Line
ln =
    into "Line"
      (oneOf
        [ defineLn
        , colorLn
        , (succeed Ignore
            |> drop line
          )
        ]
      )

defineLn : Parser Line
defineLn =
  succeed Define
    |> followedBy beginningOfLine
    |> drop (zeroOrMore space_)
    |> drop (text "#define")
    |> drop (oneOrMore space_)
    |> take xrName
    |> drop (oneOrMore space_)
    |> take xrValue
    |> drop (zeroOrMore space_)
    |> drop (eol)

colorLn : Parser Line
colorLn =
  let
    prefix = oneOf
      [ text "*."
      , text "*"
      , textNoCase "urxvt*"
      , textNoCase "xterm.vt100."
      ]
  in
    succeed Color
      |> followedBy beginningOfLine
      |> drop (zeroOrMore space_)
      |> drop prefix
      |> take xrColorName
      |> drop (zeroOrMore space_)
      |> drop (text ":")
      |> drop (zeroOrMore space_)
      |> take xrValue
      |> drop (zeroOrMore space_)
      |> drop (eol)

xrValue : Parser Value
xrValue =
  oneOf
    [ oneOf [ hexColor, rgbColor ] |> map Value
    , xrName |> map Name
    ]

xrName : Parser String
xrName =
  (concat
    [ (exactly 1 (oneOf [ char '_', letter ]))
    , (zeroOrMore (oneOf [ char '_', alphaNum ]))
    ]
    |> textOf
  )

xrColorName : Parser Theme.Key
xrColorName =
  into "Theme.Key"
    (oneOrMore alphaNum
    |> textOf
    |> andThen
      (Theme.keyByXrName
      >> Maybe.map succeed
      >> Maybe.withDefault (expected "")
      )
    )

eol : Parser ()
eol =
  succeed ()
    |> drop (zeroOrOne comment)
    |> drop (endOfLine)

comment : Parser ()
comment =
  succeed ()
    |> drop (char '!')
    |> drop (zeroOrMore anyChar)

space_ : Parser ()
space_ =
  succeed ()
  |> drop (oneOf [ char ' ', char '\t' ])

hexColor : Parser RGB255
hexColor =
  into "RGB255"
    (succeed RGB255.fromHex
      |> drop (char '#')
      |> take
        (oneOf [ exactly 6 hexDigit, exactly 3 hexDigit ]
        |> textOf
        )
      |> andThen
        (Maybe.map succeed
        >> Maybe.withDefault (expected "")
        )
    )

rgbColor : Parser RGB255
rgbColor =
  let
    component =
      between 1 2 hexDigit
      |> textOf
      |> map (Hex.fromString >> Result.withDefault 0)
  in
    into "RGB255"
      (succeed RGB255
        |> drop (text "rgb:")
        |> take component
        |> drop (char '/')
        |> take component
        |> drop (char '/')
        |> take component
      )

hexDigit : Parser Char
hexDigit =
  anyChar
  |> andThen (\c -> if Char.isHexDigit c then succeed c else expected "")
