module L06.JsonParser where

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

