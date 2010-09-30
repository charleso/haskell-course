module L06.JsonParser where

import Prelude hiding (exponent)
import Numeric
import Control.Applicative
import L01.Validation
import L03.Parser
import L06.JsonValue
import L06.MoreParser

jsonString ::
  Parser String
jsonString =
  let e = oneof "\"\\/bfnrt" ||| hex
      c = (is '\\' >> e)
          ||| satisfyAll [(/= '"'), (/= '\\')]
  in betweenCharTok '"' '"' (list c)

jsonNumber ::
  Parser Rational
jsonNumber =
  P (\i -> case readSigned readFloat i of
             [] -> Error ("Expected Rational but got " ++ show i)
             ((n, z):_) -> Value (z, n))

jsonObject ::
  Parser Assoc
jsonObject =
  let field = (,) <$> (jsonString <* charTok ':') <*> jsonValue
  in betweenSepbyComma '{' '}' field

jsonArray ::
  Parser [JsonValue]
jsonArray =
  betweenSepbyComma '[' ']' jsonValue

jsonTrue ::
  Parser String
jsonTrue =
  stringTok "true"

jsonFalse ::
  Parser String
jsonFalse =
  stringTok "false"

jsonNull ::
  Parser String
jsonNull =
  stringTok "null"

jsonValue ::
  Parser JsonValue
jsonValue =
      spaces *>
      (JsonNull <$ jsonNull
   ||| JsonTrue <$ jsonTrue
   ||| JsonFalse <$ jsonFalse
   ||| JsonArray <$> jsonArray
   ||| JsonString <$> jsonString
   ||| JsonObject <$> jsonObject
   ||| JsonRational False <$> jsonNumber)

readJsonValue ::
  FilePath
  -> IO JsonValue
readJsonValue p =
  do c <- readFile p
     case jsonValue <.> c of
       Error m -> error m
       Value a -> return a

