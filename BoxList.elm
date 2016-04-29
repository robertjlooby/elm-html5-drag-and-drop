module BoxList (..) where

import Box
import Color exposing (Color)
import Html exposing (button, div, Html, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Random.PCG as Random exposing (generate, Generator)


type alias Model =
  { boxes : List Box.Model
  , seed : Random.Seed
  }


type Action
  = AddBox


colorGenerator : Generator Color
colorGenerator =
  let
    intGenerator =
      Random.int 0 255
  in
    Random.map3 Color.rgb intGenerator intGenerator intGenerator


update : Action -> Model -> Model
update action model =
  case action of
    AddBox ->
      let
        ( color, newSeed ) =
          generate colorGenerator model.seed

        newBox =
          Box.Model color
      in
        { model
          | boxes = List.append model.boxes [ newBox ]
          , seed = newSeed
        }


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
          (List.indexedMap Box.view model.boxes)
