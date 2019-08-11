module Main exposing (main)

import SelectionSort
import Browser
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Lazy exposing (..)
import InsertionSort
import Skeleton


main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view =
            \model ->
                { title = "Sorting Algorithms"
                , body = view model
                }
        }



-- MODEL


type alias Model =
    { selectionSort : SelectionSort.Model
    , insertionSort : InsertionSort.Model
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model SelectionSort.initModel InsertionSort.initModel
    , Cmd.batch
        [ Cmd.map SelectionSortEvent SelectionSort.initCmd
        , Cmd.map InsertionSortEvent InsertionSort.initCmd
        ]
    )



-- UPDATE


type Msg
    = SelectionSortEvent SelectionSort.Msg
    | InsertionSortEvent InsertionSort.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectionSortEvent eventMsg ->
            Tuple.mapBoth
                (\selectionSort -> { model | selectionSort = selectionSort })
                (\cmd -> Cmd.map SelectionSortEvent cmd)
                (SelectionSort.update eventMsg model.selectionSort)

        InsertionSortEvent eventMsg ->
            Tuple.mapBoth
                (\insertionSort -> { model | insertionSort = insertionSort })
                (\cmd -> Cmd.map InsertionSortEvent cmd)
                (InsertionSort.update eventMsg model.insertionSort)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map SelectionSortEvent (SelectionSort.subscriptions model.selectionSort)
        , Sub.map InsertionSortEvent (InsertionSort.subscriptions model.insertionSort)
        ]



-- VIEW


view : Model -> List (Html Msg)
view model =
    [ lazy Skeleton.sidebar Skeleton.SortingAlgorithms
    , div [ class "page" ]
        [ Html.map SelectionSortEvent (SelectionSort.view model.selectionSort)
        , Html.map InsertionSortEvent (InsertionSort.view model.insertionSort)
        ]
    ]
