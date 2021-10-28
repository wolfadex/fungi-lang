module Editor.Field exposing
    ( Field
    , fromString
    , getParsed
    , getRaw
    )


type Field a
    = Field String (Result String a)


fromString : (String -> Result String a) -> String -> Field a
fromString parser raw =
    Field raw (parser raw)


getRaw : Field a -> String
getRaw (Field raw _) =
    raw


getParsed : Field a -> Result String a
getParsed (Field _ parsed) =
    parsed
