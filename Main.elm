module Main (..) where

import Box
import BoxList
import Effects exposing (Effects, Never)
import Html exposing (button, div, Html, text)
import Html.Attributes exposing (style)
import Html.Events exposing (on, onClick, onWithOptions, targetValue)
import Json.Decode exposing (at, int, string, succeed)
import Random.PCG as Random exposing (generate, Generator, independentSeed, initialSeed)
import StartApp exposing (start)
import String
import Task


type alias ID =
  Int


type alias Model =
  { boxLists : List ( ID, BoxList.Model )
  , nextId : ID
  , lastDragged : Maybe ( ID, Int )
  , seed : Random.Seed
  }


type Action
  = AddNewBoxList
  | RemoveBoxList ID
  | ModifyBoxList ID BoxList.Action
  | UpdateLastDragged ( ID, Int )
  | HandleDropped ID
  | NoOp


boxListGenerator : Generator BoxList.Model
boxListGenerator =
  Random.map (BoxList.Model []) independentSeed


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    NoOp ->
      ( model, Effects.none )

    AddNewBoxList ->
      let
        ( newBoxList, newSeed ) =
          generate boxListGenerator model.seed

        newModel =
          { model
            | boxLists = List.append model.boxLists [ ( model.nextId, newBoxList ) ]
            , nextId = model.nextId + 1
            , seed = newSeed
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

    ModifyBoxList id boxListAction ->
      let
        updateById ( id', boxList ) =
          if id' == id then
            ( id', BoxList.update boxListAction boxList )
          else
            ( id', boxList )

        newModel =
          { model | boxLists = List.map updateById model.boxLists }
      in
        ( newModel, Effects.none )

    UpdateLastDragged ( boxListId, boxIndex ) ->
      ( { model | lastDragged = Just ( boxListId, boxIndex ) }, Effects.none )

    HandleDropped id ->
      case model.lastDragged of
        Nothing ->
          ( model, Effects.none )

        Just ( fromBoxListId, boxIndex ) ->
          let
            ( draggedBox, boxListsWithoutDraggedBox ) =
              List.foldr
                (\( id', boxList ) ( maybeBox, boxLists ) ->
                  if fromBoxListId == id' then
                    let
                      droppedBox =
                        List.drop boxIndex boxList.boxes
                          |> List.head

                      listWithoutDroppedBox =
                        boxList.boxes
                          |> List.indexedMap (,)
                          |> List.filter (\( i, _ ) -> i /= boxIndex)
                          |> List.map snd

                      boxListWithoutDroppedBox =
                        { boxList | boxes = listWithoutDroppedBox }
                    in
                      ( droppedBox, ( id', boxListWithoutDroppedBox ) :: boxLists )
                  else
                    ( maybeBox, ( id', boxList ) :: boxLists )
                )
                ( Nothing, [] )
                model.boxLists
          in
            case draggedBox of
              Nothing ->
                ( model, Effects.none )

              Just box ->
                let
                  newBoxLists =
                    List.map
                      (\( id', boxList ) ->
                        if id' == id then
                          ( id', { boxList | boxes = List.append boxList.boxes [ box ] } )
                        else
                          ( id', boxList )
                      )
                      boxListsWithoutDraggedBox
                in
                  ( { model | boxLists = newBoxLists }, Effects.none )


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
    [ style [ ( "float", "left" ) ]
    , on
        "dragstart"
        (at [ "target", "id" ] string)
        (\boxId -> Signal.message address <| UpdateLastDragged ( id, String.toInt boxId |> Result.toMaybe |> Maybe.withDefault -1 ))
    , on
        "drop"
        (succeed "dropped")
        (\_ -> Signal.message address (HandleDropped id))
    , onWithOptions
        "dragover"
        { preventDefault = True, stopPropagation = True }
        (succeed "dragged over")
        (\_ -> Signal.message address NoOp)
    ]
    [ (BoxList.view (Signal.forwardTo address <| ModifyBoxList id) boxList)
    , button [ onClick address <| RemoveBoxList id ] [ text "Remove" ]
    ]


app : StartApp.App Model
app =
  let
    initialModel =
      Model [] 0 Nothing (initialSeed 0)
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
