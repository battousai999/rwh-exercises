module PrettyJSON (renderJValue) where

import Prettify(Doc(), text, double, string, series, (<>))
import SimpleJSON

renderJValue :: JValue -> Doc
renderJValue (JBool True)  = text "true"
renderJValue (JBool False) = text "false"
renderJValue JNull         = text "null"
renderJValue (JNumber num) = double num
renderJValue (JString str) = string str
renderJValue (JArray arr)  = series '[' ']' renderJValue arr

renderJValue (JObject obj) = series '{' '}' field obj
    where field (name, val) = string name 
                           <> text ": " 
                           <> renderJValue val
