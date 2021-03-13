module JsonUnParser where

import Data.List
import qualified Data.Map as M

import JsonValue

unParse :: JValue -> String
unParse (JNull)       = "null"
unParse (JBool b)     = show b
unParse (JNumber n)   = show n
unParse (JString s)   = show s
unParse (JArray list) = "[" ++ intercalate "," (map unParse list)
unParse (JObject obj) = "{" ++ intercalate "," (map f (M.toList obj)) ++ "}"
    where f (k,v) = show k ++ ":" ++ unParse v
