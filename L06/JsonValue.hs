module L06.JsonValue where

import Data.Map hiding (map)

data JsonValue =
     JsonString String
   | JsonNumber Double
   | JsonObject (Map String JsonValue)
   | JsonArray  [JsonValue]
   | JsonTrue
   | JsonFalse
   | JsonNull
  deriving (Show, Eq)

