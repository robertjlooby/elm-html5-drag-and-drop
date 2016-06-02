module Box exposing (..)

import Color exposing (Color, toRgb)
import Html exposing (div, Html)
import Html.Attributes exposing (draggable, id, style)
import Uuid exposing (Uuid)


type alias Model =
    { color : Color
    , id : Uuid
    }


view : Model -> Html msg
view model =
    let
        color =
            toRgb model.color

        colorCss =
            "rgb(" ++ toString color.red ++ "," ++ toString color.green ++ "," ++ toString color.blue ++ ")"

        boxStyle =
            style
                [ ( "backgroundColor", colorCss )
                , ( "height", "100px" )
                , ( "width", "100px" )
                , ( "float", "left" )
                , ( "margin", "5px" )
                ]
    in
        div [ boxStyle, draggable "true", id <| Uuid.toString model.id ] []
