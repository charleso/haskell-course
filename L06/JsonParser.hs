module L06.JsonParser where

import Prelude hiding (exponent)
import Data.Char
import Data.Map
import L03.Parser
import L06.JsonValue
import L06.MoreParser

jsonNull
  :: Parser JsonValue
jsonNull =
  do stringThenSpaces "null"
     return JsonNull

jsonTrue
  :: Parser JsonValue
jsonTrue =
  do stringThenSpaces "true"
     return JsonNull

jsonFalse
  :: Parser JsonValue
jsonFalse =
  do stringThenSpaces "false"
     return JsonNull

fractional
  :: Parser String
fractional =
  do is '.'
     d <- digits
     return ('.':d)

exponent ::
  Parser String
exponent =
  do e <- oneof "eE"
     s <- option '+' (oneof "+-")
     d <- digits
     return (e:s:d)

int ::
  Parser String
int =
  do s <- option [] (string "-")
     v <- string "0" ||| digits
     return (s ++ v)

jsonNumber ::
  Parser JsonValue
jsonNumber =
  do i <- int
     f <- option [] fractional
     e <- option [] exponent
     return . JsonNumber . read . concat $ [i,f,e]

isJsonChar :: Char -> Bool
isJsonChar =
  and . sequence [isAscii, isPrint, (/= '\\'), (/= '"')]

jsonChar ::
  Parser Char
jsonChar =
  satisfy isJsonChar
  ||| do is '\\'  -- escaping backslash
         is '\\'  -- escaped character
           ||| is '"'
           ||| is '/'
           ||| (is 'b' >> return '\b')
           ||| (is 'f' >> return '\f')
           ||| (is 'n' >> return '\n')
           ||| (is 'r' >> return '\r')
           ||| (is 't' >> return '\t')
           ||| hex

jsonString ::
  Parser JsonValue
jsonString =
  do s <- between quote quote (list jsonChar)
     return (JsonString s)

jsonPair ::
  Parser (String, JsonValue)
jsonPair =
  let z (JsonString s) = s
      z _              = []
  in do k   <- thenSpaces jsonString
        thenSpaces (is ':')
        v <- thenSpaces jsonValue
        return (z k, v)

jsonObject ::
  Parser JsonValue
jsonObject =
  do p <- between (charThenSpaces '{') (charThenSpaces '}') (sepby jsonPair commaThenSpaces)
     return . JsonObject $ fromList p

jsonArray ::
  Parser JsonValue
jsonArray =
  do v <- between (charThenSpaces '[') (charThenSpaces ']') (sepby (thenSpaces jsonValue) commaThenSpaces)
     return . JsonArray $ v

jsonValue ::
  Parser JsonValue
jsonValue =
  do spaces
     obj <- thenSpaces
              (jsonString
           ||| jsonNumber
           ||| jsonObject
           ||| jsonArray
           ||| jsonTrue
           ||| jsonFalse
           ||| jsonNull
           )
     return obj

