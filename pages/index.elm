module Main exposing (main)

import BinarySearch
import Browser
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Lazy exposing (..)
import LinearSearch
import Skeleton


main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view =
            \model ->
                { title = "Search Algorithms"
                , body = view model
                }
        }



-- MODEL


type alias Model =
    { linearSearch : LinearSearch.Model
    , binarySearch : BinarySearch.Model
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model LinearSearch.initModel BinarySearch.initModel
    , Cmd.batch
        [ Cmd.map LinearSearchEvent LinearSearch.initCmd
        , Cmd.map BinarySearchEvent BinarySearch.initCmd
        ]
    )



-- UPDATE


type Msg
    = LinearSearchEvent LinearSearch.Msg
    | BinarySearchEvent BinarySearch.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinearSearchEvent lsMsg ->
            Tuple.mapBoth
                (\linearSearch -> { model | linearSearch = linearSearch })
                (\cmd -> Cmd.map LinearSearchEvent cmd)
                (LinearSearch.update lsMsg model.linearSearch)

        BinarySearchEvent bsMsg ->
            Tuple.mapBoth
                (\binarySearch -> { model | binarySearch = binarySearch })
                (\cmd -> Cmd.map BinarySearchEvent cmd)
                (BinarySearch.update bsMsg model.binarySearch)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map LinearSearchEvent (LinearSearch.subscriptions model.linearSearch)
        , Sub.map BinarySearchEvent (BinarySearch.subscriptions model.binarySearch)
        ]



-- VIEW


view : Model -> List (Html Msg)
view model =
    [ lazy Skeleton.sidebar Skeleton.SearchAlgorithms
    , div [ class "page" ]
        [ Html.map LinearSearchEvent (LinearSearch.view model.linearSearch)
        , Html.map BinarySearchEvent (BinarySearch.view model.binarySearch)
        ]
    ]
