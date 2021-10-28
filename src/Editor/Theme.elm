module Editor.Theme exposing (error, text)

import Element exposing (Attribute, Color, Element)
import Element.Border
import Element.Input exposing (Label, Placeholder)


text :
    List (Attribute msg)
    ->
        { onChange : String -> msg
        , text : String
        , placeholder : Maybe (Placeholder msg)
        , label : Label msg
        }
    -> Element msg
text attrs =
    Element.Input.text
        ([]
            ++ attrs
        )


error : Color
error =
    Element.rgb 1 0 0
