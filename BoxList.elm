module BoxList exposing (..)

import Box
import Color exposing (Color)
import Html exposing (button, div, Html, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Random.Pcg as Random exposing (step, Generator)
import Uuid exposing (Uuid, uuidGenerator)


type alias Model =
    { boxes : List Box.Model
    , seed : Random.Seed
    }


type Msg
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


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddBox ->
            let
                ( newBox, newSeed ) =
                    step boxGenerator model.seed
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


view : Model -> Html Msg
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
        div [ boxesStyle ] <|
            List.append [ button [ onClick AddBox ] [ text "+" ] ]
                (List.map Box.view model.boxes)
