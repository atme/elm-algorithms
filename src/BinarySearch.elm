module BinarySearch exposing (main)

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


lookingForGenerator : Random.Generator Int
lookingForGenerator = Random.int 0 9



--MODEL


type alias Model =
    { lookingFor : Int
    , previous : List Int
    , status : Status
    , remaining : List Int
    }


type Status
    = Idle
    | Selected (List Int)
    | Current (List Int) Int (List Int)
    | Found Int
    | NotFound


init : () -> (Model, Cmd Msg)
init _ = 
    ( Model 0 [] Idle []
    , Random.generate NewList listGenerator
    )



-- UPDATE


type Msg 
    = Reset
    | Run
    | NextSelectedStep (List Int) Time.Posix
    | NextCurrentStep (List Int) Int (List Int) Time.Posix
    | NewList (List Int)
    | NewLookingFor Int


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Reset ->
            init ()

        Run ->
            ( { model | status = Selected model.remaining, remaining = []}
            , Cmd.none
            )

        NextSelectedStep list _ ->
            let
                index =
                    List.length list // 2
            in
                case List.head ( List.drop index list ) of
                    Just current ->
                        (
                            { model
                            | status = Current
                                ( List.take index list )
                                current
                                ( List.drop (index + 1) list )
                            }
                            , Cmd.none
                        )

                    Nothing ->
                        (
                            { model
                            | status = NotFound
                            , remaining = list ++ model.remaining
                            }
                        , Cmd.none
                        )

        NextCurrentStep leftList current rightList _ ->
            if current == model.lookingFor then
                (
                    { model
                    | previous = model.previous ++ leftList
                    , status = Found current
                    , remaining = rightList ++ model.remaining
                    }
                , Cmd.none
                )
            else if current > model.lookingFor then
                (
                    { model
                    | status = Selected leftList
                    , remaining = (current :: []) ++ rightList ++ model.remaining
                    }
                , Cmd.none
                )
            else
                (
                    { model
                    | previous = model.previous ++ leftList ++ (current :: [])
                    , status = Selected rightList
                    }
                , Cmd.none
                )

        NewList list ->
            ( { model | remaining = List.sort list }
            , Random.generate NewLookingFor lookingForGenerator
            )

        NewLookingFor index ->
            let
                lookingFor =
                    model.remaining
                        |> List.drop index
                        |> List.head
                        |> Maybe.withDefault (index * 10)
            in
            ( { model | lookingFor = lookingFor }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.status of
        Selected list ->
            Time.every 1000 (NextSelectedStep list)

        Current leftList current rightList ->
            Time.every 1000 ( NextCurrentStep leftList current rightList )

        _ ->
            Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        foundText =
            case model.status of
                Found _ ->
                    " | The index: " ++ String.fromInt (List.length model.previous)
                NotFound ->
                    " | The value is not in the list"
                _ ->
                    ""
    in
        div [ class "algorithm" ]
            [ h1 [] [ text "Binary Search" ]
            , button
                [ onClick Run
                , disabled (model.status /= Idle)
                ]
                [ text "Run" ]
            , button [ onClick Reset ] [ text "Reset" ]
            , h2 [] [ text ("We are looking for: " ++ String.fromInt model.lookingFor)
                    , text (foundText)
                    ]
            , viewSquares model
            ]


viewSquares : Model -> Html Msg
viewSquares model =
    let
        squares =
            case model.status of
                Idle ->
                    List.map viewEmptySquare model.previous
                 ++ List.map viewEmptySquare model.remaining

                Selected list ->
                    List.map viewEmptySquare model.previous
                 ++ List.map viewSelectedSquare list
                 ++ List.map viewEmptySquare model.remaining

                Current leftList current rightList ->
                    List.map viewEmptySquare model.previous
                 ++ List.map viewSelectedSquare leftList
                 ++ List.singleton (viewCurrentSquare current)
                 ++ List.map viewSelectedSquare rightList
                 ++ List.map viewEmptySquare model.remaining

                Found found ->
                    List.map viewEmptySquare model.previous
                 ++ List.singleton (viewFoundSquare found)
                 ++ List.map viewEmptySquare model.remaining

                NotFound ->
                    List.map viewEmptySquare model.previous
                 ++ List.map viewEmptySquare model.remaining
    in
    div [ class "squares" ] squares


viewEmptySquare : Int -> Html Msg
viewEmptySquare number =
    viewSquare "" (String.fromInt number)


viewCurrentSquare : Int -> Html Msg
viewCurrentSquare number =
    viewSquare "upper bright" (String.fromInt number)


viewSelectedSquare : Int -> Html Msg
viewSelectedSquare number =
    viewSquare "bright" (String.fromInt number)


viewFoundSquare : Int -> Html Msg
viewFoundSquare number =
    viewSquare "dark" (String.fromInt number)


viewSquare : String -> String -> Html Msg
viewSquare additionalClass value =
    div [ class "square", class additionalClass ] [ text value ]
