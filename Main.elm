module Main exposing (..)

import Box
import BoxList
import Html exposing (button, div, Html, text)
import Html.App as App
import Html.Attributes exposing (style)
import Html.Events exposing (on, onClick, onWithOptions)
import Json.Decode as Decode exposing (at, customDecoder, Decoder, int, string, succeed)
import Random.Pcg as Random exposing (step, Generator, independentSeed, initialSeed)
import Uuid exposing (Uuid)


type alias ID =
    Int


type alias Model =
    { boxLists : List ( ID, BoxList.Model )
    , nextId : ID
    , lastDragged : Maybe Uuid
    , seed : Random.Seed
    }


type Msg
    = AddNewBoxList
    | RemoveBoxList ID
    | ModifyBoxList ID BoxList.Msg
    | UpdateLastDragged Uuid
    | HandleDropped ID
    | NoOp


boxListGenerator : Generator BoxList.Model
boxListGenerator =
    Random.map (BoxList.Model []) independentSeed


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        AddNewBoxList ->
            let
                ( newBoxList, newSeed ) =
                    step boxListGenerator model.seed

                newModel =
                    { model
                        | boxLists = List.append model.boxLists [ ( model.nextId, newBoxList ) ]
                        , nextId = model.nextId + 1
                        , seed = newSeed
                    }
            in
                ( newModel, Cmd.none )

        RemoveBoxList id ->
            let
                newModel =
                    { model
                        | boxLists = List.filter (\( id', _ ) -> id' /= id) model.boxLists
                    }
            in
                ( newModel, Cmd.none )

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
                ( newModel, Cmd.none )

        UpdateLastDragged boxId ->
            ( { model | lastDragged = Just boxId }, Cmd.none )

        HandleDropped droppedBoxListId ->
            case model.lastDragged of
                Nothing ->
                    ( model, Cmd.none )

                Just boxId ->
                    let
                        partitionedLists =
                            List.map (\( id, boxList ) -> ( id, BoxList.removeBox boxId boxList )) model.boxLists

                        draggedBox =
                            List.filterMap (\( _, ( draggedBox, _ ) ) -> draggedBox) partitionedLists
                                |> List.head
                    in
                        case draggedBox of
                            Nothing ->
                                ( model, Cmd.none )

                            Just box ->
                                let
                                    newBoxLists =
                                        List.foldr
                                            (\( id, ( _, boxList ) ) boxLists ->
                                                if id == droppedBoxListId then
                                                    ( id, { boxList | boxes = List.append boxList.boxes [ box ] } ) :: boxLists
                                                else
                                                    ( id, boxList ) :: boxLists
                                            )
                                            []
                                            partitionedLists

                                    newModel =
                                        { model | boxLists = newBoxLists }
                                in
                                    ( newModel, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ div [] <| List.map boxListView model.boxLists
        , button [ onClick AddNewBoxList ] [ text "Add new list" ]
        ]


uuidDecoder : Decoder Uuid
uuidDecoder =
    customDecoder string (Result.fromMaybe "failed to parse Uuid" << Uuid.fromString)


boxListView : ( ID, BoxList.Model ) -> Html Msg
boxListView ( id, boxList ) =
    div
        [ style [ ( "float", "left" ) ]
        , on "dragstart"
            (Decode.map UpdateLastDragged (at [ "target", "id" ] uuidDecoder))
        , on "drop"
            (succeed <| HandleDropped id)
        , onWithOptions "dragover"
            { preventDefault = True, stopPropagation = True }
            (succeed NoOp)
        ]
        [ App.map (ModifyBoxList id) (BoxList.view boxList)
        , button [ onClick <| RemoveBoxList id ] [ text "Remove" ]
        ]


main : Program Never
main =
    let
        initialModel =
            Model [] 0 Nothing (initialSeed 0)
    in
        App.program
            { init = ( initialModel, Cmd.none )
            , view = view
            , update = update
            , subscriptions = (\_ -> Sub.none)
            }
