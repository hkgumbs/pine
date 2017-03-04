{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.ErlangCore.String
  ( character, bitString
  ) where


import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as Text

import Generate.ErlangCore.Builder as Core


character :: Text -> Core.Expr
character =
  Core.Chr . Text.head . unescape


bitString :: Text -> Core.Expr
bitString =
  Core.BitString . encodeUtf8 . unescape


unescape :: Text -> Text
unescape =
  Text.replace "\\'" "'"
  . Text.replace "\\\"" "\""
  . Text.replace "\\n" "\n"
  . Text.replace "\\t" "\t"
  . Text.replace "\\\\" "\\"
