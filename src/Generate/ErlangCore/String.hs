{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.ErlangCore.String
  ( character, bitString
  ) where


import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as Text
import qualified Data.Char as Char
import qualified Numeric

import Generate.ErlangCore.Builder as Core


character :: Text -> Core.Expr
character =
  Core.Chr . Text.head . unescape


bitString :: Text -> Core.Expr
bitString =
  Core.BitString . encodeUtf8 . unescape


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
