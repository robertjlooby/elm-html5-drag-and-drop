module Box (..) where

import Html exposing (div, Html)
import Html.Attributes exposing (draggable, style)


type alias Model =
  {}


view : Model -> Html
view model =
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
    div [ boxStyle, draggable "true" ] []
