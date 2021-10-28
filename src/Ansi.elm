module Ansi exposing (..)


clearEntireLine : String -> String
clearEntireLine str =
    "\u{001B}[2K" ++ str
