module Path exposing (..)

import String exposing (right)


type alias Path =
    String


join : Path -> Path -> Path
join left right =
    let
        finalLeft =
            if String.endsWith "/" left then
                left

            else
                left ++ "/"

        finalRight =
            if String.startsWith "/" right then
                String.dropLeft 1 right

            else
                right
    in
    finalLeft ++ finalRight


parentDirectory : Path -> Path
parentDirectory current =
    join current "../"


getFileDirectory : Path -> Path
getFileDirectory =
    String.split "/" >> List.reverse >> List.drop 1 >> List.reverse >> String.join "/"
