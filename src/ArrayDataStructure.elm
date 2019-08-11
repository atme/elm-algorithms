module ArrayDataStructure exposing (Model, Msg, initCmd, initModel, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random
import Task
import Time


main =
    Browser.element
        { init = \() -> ( initModel, initCmd )
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


listGenerator : Random.Generator (List Int)
listGenerator =
    Random.list 10 (Random.int 10 99)



-- MODEL


type alias Model =
    { index : Maybe Int
    , array : Array
    }


type Array
    = Idle (List Int)
    | Selected (List Int) Int (List Int)


initModel : Model
initModel =
    Model (Just 0) (Idle [])


initCmd : Cmd Msg
initCmd =
    Random.generate NewList listGenerator



-- UPDATE


type Msg
    = ChangeIndex String
    | Search
    | NewList (List Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeIndex string ->
            if String.length string < 2 then
                update Search { model | index = String.toInt string }

            else
                case String.toInt string of
                    Just index ->
                        update Search { model | index = Just index }

                    Nothing ->
                        ( model, Cmd.none )

        Search ->
            let
                list =
                    case model.array of
                        Idle array ->
                            array

                        Selected leftList value rightList ->
                            leftList ++ (value :: []) ++ rightList
            in
            case model.index of
                Nothing ->
                    ( { model | array = Idle list }
                    , Cmd.none
                    )

                Just index ->
                    case List.head (List.drop index list) of
                        Just value ->
                            let
                                leftList =
                                    List.take index list

                                rightList =
                                    List.drop (index + 1) list
                            in
                            ( { model
                                | array = Selected leftList value rightList
                              }
                            , Cmd.none
                            )

                        Nothing ->
                            ( { model | array = Idle list }
                            , Cmd.none
                            )

        NewList list ->
            update Search { model | array = Idle list }



-- VIEW


view : Model -> Html Msg
view model =
    let
        index =
            case model.index of
                Just number ->
                    String.fromInt number

                Nothing ->
                    ""
    in
    div [ class "algorithm" ]
        [ h1 [] [ text "Array" ]
        , h2 []
            [ text "Get index:"
            , input
                [ onInput ChangeIndex
                , value index
                , maxlength 1
                , class "inline-input"
                ]
                []
            ]
        , viewSquares model
        , ul []
            [ li [] [ text "+ Access to any index is fast O(1)" ]
            , li [] [ text " - The array size is fixed" ]
            ]
        ]


viewSquares : Model -> Html Msg
viewSquares model =
    let
        squares =
            case model.array of
                Idle list ->
                    List.map viewEmptySquare list

                Selected leftList value rightList ->
                    List.map viewEmptySquare leftList
                        ++ List.singleton (viewCurrentSquare value)
                        ++ List.map viewEmptySquare rightList
    in
    div [ class "squares" ] squares


viewEmptySquare : Int -> Html Msg
viewEmptySquare number =
    viewSquare "" (String.fromInt number)


viewCurrentSquare : Int -> Html Msg
viewCurrentSquare number =
    viewSquare "upper" (String.fromInt number)


viewSquare : String -> String -> Html Msg
viewSquare additionalClass value =
    div [ class "square", class additionalClass ] [ text value ]
