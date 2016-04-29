module Box (..) where

import Html exposing (div, Html)
import Html.Attributes exposing (draggable, id, style)


type alias Model =
  {}


view : Int -> Model -> Html
view index model =
  let
    boxStyle =
      style
        [ ( "backgroundColor", "red" )
        , ( "height", "100px" )
        , ( "width", "100px" )
        , ( "float", "left" )
        , ( "margin", "5px" )
        ]
  in
    div [ boxStyle, draggable "true", id <| toString index ] []
