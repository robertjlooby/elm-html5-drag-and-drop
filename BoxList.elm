module BoxList (..) where

import Box
import Html exposing (button, div, Html, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


type alias Model =
  { boxes : List Box.Model
  }


type Action
  = AddBox


update : Action -> Model -> Model
update action model =
  case action of
    AddBox ->
      { model | boxes = List.append model.boxes [ Box.Model ] }


view : Signal.Address Action -> Model -> Html
view address model =
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
    div
      [ boxesStyle ]
      <| List.append
          [ button [ onClick address AddBox ] [ text "+" ] ]
          (List.map Box.view model.boxes)
