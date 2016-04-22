module Main (..) where

import Box
import BoxList
import Effects exposing (Effects, Never)
import Html exposing (button, div, Html, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import StartApp exposing (start)
import Task


type alias ID =
  Int


type alias Model =
  { boxLists : List ( ID, BoxList.Model )
  , nextId : ID
  }


type Action
  = AddNewBoxList
  | RemoveBoxList ID


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    AddNewBoxList ->
      let
        newModel =
          { model
            | boxLists = List.append model.boxLists [ ( model.nextId, BoxList.Model [] ) ]
            , nextId = model.nextId + 1
          }
      in
        ( newModel, Effects.none )

    RemoveBoxList id ->
      let
        newModel =
          { model
            | boxLists = List.filter (\( id', _ ) -> id' /= id) model.boxLists
          }
      in
        ( newModel, Effects.none )


view : Signal.Address Action -> Model -> Html
view address model =
  div
    []
    [ div [] <| List.map (boxListView address) model.boxLists
    , button [ onClick address AddNewBoxList ] [ text "Add new list" ]
    ]


boxListView : Signal.Address Action -> ( ID, BoxList.Model ) -> Html
boxListView address ( id, boxList ) =
  div
    [ style [ ( "float", "left" ) ] ]
    [ (BoxList.view boxList)
    , button [ onClick address <| RemoveBoxList id ] [ text "Remove" ]
    ]


app : StartApp.App Model
app =
  let
    initialModel =
      Model [] 0
  in
    start
      { init = ( initialModel, Effects.none )
      , view = view
      , update = update
      , inputs = []
      }


main : Signal Html
main =
  app.html


port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks
