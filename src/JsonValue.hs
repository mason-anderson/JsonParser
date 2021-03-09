module JsonValue where

import qualified Data.Map as M

data JValue
    =  JNull
    | JBool Bool
    | JNumber Integer
    | JString String
    | JArray [JValue]
    | JObject (M.Map String JValue)
    deriving (Show, Eq)
