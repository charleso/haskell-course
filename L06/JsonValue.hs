module L06.JsonValue where

import Data.Map hiding (map)

type Assoc = [(String, JsonValue)]

data JsonValue =
     JsonString String
   | JsonRational  Bool !Rational
   | JsonObject Assoc
   | JsonArray  [JsonValue]
   | JsonTrue
   | JsonFalse
   | JsonNull
  deriving (Show, Eq)

