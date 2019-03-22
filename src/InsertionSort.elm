module InsertionSort exposing (main)

import Browser
import Html exposing (Html, div, text, h1, button, h2)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick)
import Random
import Time


main = 
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


listGenerator : Random.Generator (List Int)
listGenerator = Random.list 10 (Random.int 10 99)



-- MODEL


type Model
    = Idle (List Int)
    | NewLoop (List Int) (List Int)
    | SortValue (List Int) Int (List Int) (List Int)
    | Sorted (List Int)


init : () -> (Model, Cmd Msg)
init _ =
    ( Idle []
    , Random.generate NewList listGenerator
    )



-- UPDATE


type Msg
    = Reset
    | Run (List Int)
    | NewLoopStep (List Int) (List Int) Time.Posix
    | SortValueStep (List Int) Int (List Int) (List Int) Time.Posix
    | NewList (List Int)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Reset ->
            init ()

        Run list ->
            ( NewLoop [] list
            , Cmd.none
            )

        NewLoopStep sorted unsorted _ ->
            case List.head unsorted of
                Just current ->
                    ( SortValue sorted current [] (List.drop 1 unsorted)
                    , Cmd.none
                    )

                Nothing ->
                    ( Sorted sorted
                    , Cmd.none
                    )

        SortValueStep remaining current previous unsorted _ ->
            case List.head (List.reverse remaining) of
                Just number ->
                    if number > current then
                        ( SortValue
                            ( remaining
                                |> List.reverse
                                |> List.drop 1
                                |> List.reverse
                            )
                            current
                            ( (number :: []) ++ previous )
                            unsorted
                        , Cmd.none
                        )
                    else
                        ( NewLoop
                            ( remaining ++ (current :: []) ++ previous )
                            unsorted
                        , Cmd.none
                        )

                Nothing ->
                    ( NewLoop
                        ( (current :: []) ++ previous )
                        unsorted
                    , Cmd.none
                    )

        NewList list ->
            ( Idle list
            , Cmd.none
            )




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        NewLoop sorted unsorted ->
            Time.every 1000 (NewLoopStep sorted unsorted)

        SortValue remaining current previous unsorted ->
            Time.every 1000 (SortValueStep remaining current previous unsorted)

        _ ->
            Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        runButtonAttributes =
            case model of
                Idle list ->
                    [ onClick (Run list) ]
                _ ->
                    [ disabled True ]
    in
    div [ class "algorithm" ]
        [ h1 [] [ text "Insertion Sort" ]
        , button runButtonAttributes [ text "Run" ]
        , button [ onClick Reset ] [ text "Reset" ]
        , h2 [] []
        , viewSquares model
        ]


viewSquares : Model -> Html Msg
viewSquares model =
    let
        squares =
            case model of
                Idle list ->
                    List.map viewEmptySquare list

                NewLoop sorted unsorted ->
                    List.map viewSortedSquare sorted
                 ++ List.map viewEmptySquare unsorted

                SortValue remaining current previous unsorted ->
                    List.map viewSortedSquare remaining
                 ++ List.singleton (viewCurrentSquare current)
                 ++ List.map viewSortedSquare previous
                 ++ List.map viewEmptySquare unsorted

                Sorted list ->
                    List.map viewEmptySquare list
    in
    div [ class "squares" ] squares


viewEmptySquare : Int -> Html Msg
viewEmptySquare number =
    viewSquare "" (String.fromInt number)


viewSortedSquare : Int -> Html Msg
viewSortedSquare number =
    viewSquare "bright" (String.fromInt number)


viewCurrentSquare : Int -> Html Msg
viewCurrentSquare number =
    viewSquare "upper" (String.fromInt number)


viewFoundSquare : Int -> Html Msg
viewFoundSquare number =
    viewSquare "dark" (String.fromInt number)

viewSquare : String -> String -> Html Msg
viewSquare additionalClass value =
    div [ class "square", class additionalClass ] [ text value ]