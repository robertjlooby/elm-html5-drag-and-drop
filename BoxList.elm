module BoxList (..) where

import Box
import Color exposing (Color)
import Html exposing (button, div, Html, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Random.PCG as Random exposing (generate, Generator)
import Uuid exposing (Uuid, uuidGenerator)


type alias Model =
  { boxes : List Box.Model
  , seed : Random.Seed
  }


type Action
  = AddBox


boxGenerator : Generator Box.Model
boxGenerator =
  let
    intGenerator =
      Random.int 0 255

    colorGenerator =
      Random.map3 Color.rgb intGenerator intGenerator intGenerator
  in
    Random.map2 Box.Model colorGenerator uuidGenerator


update : Action -> Model -> Model
update action model =
  case action of
    AddBox ->
      let
        ( newBox, newSeed ) =
          generate boxGenerator model.seed
      in
        { model
          | boxes = List.append model.boxes [ newBox ]
          , seed = newSeed
        }


removeBox : Uuid -> Model -> ( Maybe Box.Model, Model )
removeBox id model =
  let
    ( removedBox, listWithoutRemovedBox ) =
      List.foldr
        (\box ( maybeBox, list ) ->
          if box.id == id then
            ( Just box, list )
          else
            ( maybeBox, box :: list )
        )
        ( Nothing, [] )
        model.boxes
  in
    ( removedBox, { model | boxes = listWithoutRemovedBox } )


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
