module Box (..) where

import Color exposing (Color, toRgb)
import Html exposing (div, Html)
import Html.Attributes exposing (draggable, id, style)


type alias Model =
  { color : Color
  }


view : Int -> Model -> Html
view index model =
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
    div [ boxStyle, draggable "true", id <| toString index ] []
