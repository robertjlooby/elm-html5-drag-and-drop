module BoxList (..) where

import Box
import Html exposing (div, Html)
import Html.Attributes exposing (style)


type alias Model =
  { boxes : List Box.Model
  }


view : Model -> Html
view model =
  let
    boxesStyle =
      style
        [ ( "border", "black solid" )
        , ( "overflow", "auto" )
        , ( "width", "110px" )
        , ( "height", "100%" )
        , ( "margin", "5px" )
        , ( "min-height", "100px" )
        ]
  in
    div [ boxesStyle ] <| List.map Box.view model.boxes
