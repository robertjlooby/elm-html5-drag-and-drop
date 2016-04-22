module Main (..) where

import Box
import BoxList
import Effects exposing (Effects, Never)
import Html exposing (div, Html, text)
import Html.Attributes exposing (style)
import StartApp exposing (start)
import Task


type alias Model =
  { boxLists : List BoxList.Model
  }


type Action
  = None


update : Action -> Model -> ( Model, Effects Action )
update action model =
  ( model, Effects.none )


view : Signal.Address Action -> Model -> Html
view address model =
  div [] <| List.map BoxList.view model.boxLists


app : StartApp.App Model
app =
  let
    initialModel =
      Model []
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
