module BoxList (..) where

import Box
import Html exposing (button, div, Html, text)
import Html.Attributes exposing (style)
import Html.Events exposing (on, onClick, onWithOptions, targetValue)
import Json.Decode exposing (at, string, succeed)


type alias Model =
  { boxes : List Box.Model
  }


type Action
  = AddBox
  | NoOp


update : Action -> Model -> Model
update action model =
  case action of
    AddBox ->
      { model | boxes = List.append model.boxes [ Box.Model ] }

    NoOp ->
      model


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
      [ boxesStyle
      , on
          "drop"
          (at [ "dataTransfer", "draggedId" ] string)
          (\id -> Signal.message address (Debug.log id AddBox))
      , onWithOptions
          "dragover"
          { preventDefault = True, stopPropagation = True }
          (succeed "")
          (\_ -> Signal.message address NoOp)
      ]
      <| List.append
          [ button [ onClick address AddBox ] [ text "+" ] ]
          (List.map Box.view model.boxes)
