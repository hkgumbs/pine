{-# LANGUAGE OverloadedStrings #-}
module Generate.ErlangCore.Constant
  ( literal
  ) where

import Data.Text (Text)
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text as Text
import qualified Data.Char as Char
import qualified Numeric

import qualified AST.Literal as Literal
import qualified Generate.ErlangCore.Builder as Core


literal :: Literal.Literal -> Core.Constant
literal literal =
  case literal of
    Literal.Chr c ->
      Core.Char (Text.head (unescape c))

    Literal.Str text ->
      Core.BitString (Encoding.encodeUtf8 (unescape text))

    Literal.IntNum n ->
      Core.Int n

    Literal.FloatNum n ->
      Core.Float n

    Literal.Boolean b ->
      Core.Atom $ if b then "true" else "false"



-- TEXT ENCODING


data Escaping
  = No
  | Yes
  | Unicode String


unescape :: Text -> Text
unescape =
  snd . Text.foldl unescapeHelp (No, Text.empty)


unescapeHelp :: (Escaping, Text) -> Char -> (Escaping, Text)
unescapeHelp (escaping, textSoFar) next =
  let
    appendFinal =
      (,) No . Text.append textSoFar . Text.singleton
  in
    case (escaping, next) of

      -- unicode sequence (i.e. \u1F1F)
      (Yes, 'u') -> (Unicode "", textSoFar)
      (Unicode hex, _) | length hex < 3 ->
        (Unicode (hex ++ [next]), textSoFar)
      (Unicode hex, _) ->
        appendFinal . Char.chr
        . fst . head . Numeric.readHex $ hex ++ [next]

      -- regular escape codes
      (Yes, 'b') -> appendFinal '\b'
      (Yes, 'f') -> appendFinal '\f'
      (Yes, 'n') -> appendFinal '\n'
      (Yes, 'r') -> appendFinal '\r'
      (Yes, 't') -> appendFinal '\t'
      (Yes, 'v') -> appendFinal '\v'

      -- start escaping
      (No, '\\') -> (Yes, textSoFar)

      -- normal string character
      _ -> appendFinal next
