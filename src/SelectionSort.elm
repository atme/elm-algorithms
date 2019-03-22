module SelectionSort exposing (main)

import Browser
import Html exposing (Html, div, text, h1, button, h2)
import Html.Attributes exposing (class)
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
    | Running RunningAlias


type alias RunningAlias =
    { sorted : List Int
    , previous : List Int
    , selected : Int
    , middle : List Int
    , current : Maybe Int
    , remaining : List Int
    }

init : () -> (Model, Cmd Msg)
init _ =
    ( Idle []
    , Random.generate NewList listGenerator
    )



-- UPDATE


type Msg 
    = Reset
    | Run
    | NextStep RunningAlias Time.Posix
    | NewList (List Int)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Reset ->
            ( model
            , Random.generate NewList listGenerator
            )

        Run ->
            case model of
                Idle list ->
                    ( initRun list
                    , Cmd.none
                    )

                Running _ ->
                    ( model
                    , Cmd.none
                    )

        NextStep running newTime ->
            ( nextCurrent (switchSelect running)
            , Cmd.none
            )

        NewList list ->
            ( Idle list
            , Cmd.none
            )


initRun : List Int -> Model
initRun list =
    case List.head list of
        Just selected ->
            Running <| RunningAlias
                []
                []
                selected
                []
                Nothing
                (List.drop 1 list)

        Nothing ->
            Idle list


switchSelect : RunningAlias -> RunningAlias
switchSelect running =
    case running.current of
        Just current ->
            if running.selected > current then
                { running
                | previous = running.previous
                        ++ ( running.selected :: running.middle )
                , selected = current
                , middle = []
                , current = Nothing
                }
            else
                running

        Nothing ->
            running


nextCurrent : RunningAlias -> Model
nextCurrent running =
    case List.head running.remaining of
        Just number ->
            Running
                { running
                | middle = running.middle ++ toList running.current
                , current = Just number
                , remaining = List.drop 1 running.remaining
                }

        Nothing ->
            nextLoop running


nextLoop : RunningAlias -> Model
nextLoop running =
    let
        current =
            case running.current of
                Just number ->
                    number :: []
                
                Nothing ->
                    []

        remaining =
            (List.drop 1 running.previous) 
         ++ (List.take 1 running.previous)
         ++ running.middle
         ++ current
         ++ running.remaining

        sorted = running.sorted ++ List.singleton running.selected
    in
        case List.head remaining of
            Just selected ->
                Running
                    { running
                    | sorted = sorted
                    , previous = []
                    , selected = selected
                    , middle = []
                    , current = Nothing
                    , remaining = List.drop 1 remaining
                    }

            Nothing ->
                Idle sorted


toList : Maybe a -> List a
toList maybeValue =
    case maybeValue of
        Just value ->
            List.singleton value

        Nothing ->
            []




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Running running ->
            Time.every 1000 (NextStep running)
        _ ->
            Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "algorithm" ]
        [ h1 [] [ text "Selection Sort" ]
        , button [ onClick Run ] [ text "Run" ]
        , button [ onClick Reset ] [ text "Reset" ]
        , h2 [] []
        , viewSquares model
        ]


viewSquares : Model -> Html Msg
viewSquares model =
    case model of
        Idle list ->
            div [ class "squares" ] (List.map viewEmptySquare list)

        Running running ->
            let
                current =
                    case running.current of
                        Just number ->
                            List.singleton (viewCurrentSquare number)
                        
                        Nothing ->
                            []

                squares =
                    List.map viewSortedSquare running.sorted
                 ++ List.map viewEmptySquare running.previous
                 ++ List.singleton (viewFoundSquare running.selected)
                 ++ List.map viewEmptySquare running.middle
                 ++ current
                 ++ List.map viewEmptySquare running.remaining
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