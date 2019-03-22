module LinearSearch exposing (main)

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
lookingForGenerator = Random.int 1 10



--MODEL

{-| This type of the model guarantier us,
    that the model cannot reach an impossible state.
-}
type alias Model =
    { lookingFor : Int
    , previous : List Int
    , status : Status
    , remaining : List Int
    }


type Status
    = Idle
    | Running Int
    | Found Int
    | NotFound


resetModel : (Model, Cmd Msg)
resetModel =
    ( Model 0 [] Idle []
    , Random.generate NewList listGenerator
    )


init : () -> (Model, Cmd Msg)
init _ = resetModel



-- UPDATE


type Msg 
    = Reset
    | Run
    | NextStep Int Time.Posix
    | NewList (List Int)
    | NewLookingFor Int


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Reset ->
            resetModel

        Run ->
            case List.head model.remaining of
                Just current ->
                    (
                        { model
                        | previous = []
                        , status = Running current
                        , remaining = List.drop 1 model.remaining
                        }
                    ,
                    Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        NextStep current newTime ->
            if current == model.lookingFor then
                ( { model | status = Found current }
                , Cmd.none
                )
            else
                case List.head model.remaining of
                    Just newCurrent ->
                        (
                            { model
                            | previous = model.previous ++ (current :: [])
                            , status = Running newCurrent
                            , remaining = List.drop 1 model.remaining
                            }
                        , Cmd.none
                        )

                    Nothing ->
                        (
                            { model
                            | previous = model.previous ++ (current :: [])
                            , status = NotFound
                            }
                        , Cmd.none
                        )


        NewList list ->
            let
                sortedList =
                    List.sort list
            in
            ( { model | previous = [], status = Idle, remaining = sortedList }
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
        Running data ->
            Time.every 1000 (NextStep data)
        _ ->
            Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        foundText =
            case model.status of
                Found _ ->
                    " | The index: " ++String.fromInt (List.length model.previous)
                NotFound ->
                    "The value is not in the list"
                _ ->
                    ""
    in
        div [ class "algorithm" ]
            [ h1 [] [ text "Linear Search" ]
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

                Running current ->
                    List.map viewEmptySquare model.previous
                 ++ List.singleton (viewCurrentSquare current)
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
    viewSquare "upper" (String.fromInt number)


viewFoundSquare : Int -> Html Msg
viewFoundSquare number =
    viewSquare "dark" (String.fromInt number)

viewSquare : String -> String -> Html Msg
viewSquare additionalClass value =
    div [ class "square", class additionalClass ] [ text value ]