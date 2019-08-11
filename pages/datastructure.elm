module Main exposing (main)

import ArrayDataStructure
import Browser
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Lazy exposing (..)
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
    { array : ArrayDataStructure.Model }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model ArrayDataStructure.initModel
    , Cmd.batch
        [ Cmd.map ArrayEvent ArrayDataStructure.initCmd
        ]
    )



-- UPDATE


type Msg
    = ArrayEvent ArrayDataStructure.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ArrayEvent eventMsg ->
            Tuple.mapBoth
                (\array -> { model | array = array })
                (\cmd -> Cmd.map ArrayEvent cmd)
                (ArrayDataStructure.update eventMsg model.array)




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
    -- Sub.batch
    --     [ Sub.map ArrayEvent (ArrayDataStructure.subscriptions model.selectionSort)
    --     ]



-- VIEW


view : Model -> List (Html Msg)
view model =
    [ lazy Skeleton.sidebar Skeleton.DataStructures
    , div [ class "page" ]
        [ Html.map ArrayEvent (ArrayDataStructure.view model.array)
        ]
    ]
