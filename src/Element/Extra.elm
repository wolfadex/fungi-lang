module Element.Extra exposing (id)

import Element exposing (Attribute)
import Html.Attributes


id : String -> Attribute msg
id =
    Html.Attributes.id >> Element.htmlAttribute
