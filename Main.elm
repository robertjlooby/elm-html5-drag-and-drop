module Main (..) where

import Box
import BoxList
import Html exposing (div, Html, text)
import Html.Attributes exposing (style)


type alias Model =
  { boxLists : List BoxList.Model
  }


main : Html
main =
  view
    <| Model
        [ BoxList.Model [ Box.Model, Box.Model, Box.Model ]
        , BoxList.Model [ Box.Model, Box.Model ]
        ]


view : Model -> Html
view model =
  div [] <| List.map BoxList.view model.boxLists
