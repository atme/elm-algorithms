module LinearSearch exposing (main)

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


lookingForGenerator : Random.Generator Int
lookingForGenerator = Random.int 0 9



--MODEL

{- This type of the model guarantier us,
   that the model cannot reach an impossible state.
-}
type alias Model =
    { previous : List Int
    , status : Status
    , others : List Int
    }


type Status
    = Idle Int
    | Running RunningAlias
    | Found Int


type alias RunningAlias = 
    { current: Int
    , remaining: List Int
    , lookingFor: Int
    }


resetModel : (Model, Cmd Msg)
resetModel =
    ( Model [] (Idle 0) []
    , Random.generate NewList listGenerator
    )


init : () -> (Model, Cmd Msg)
init _ = resetModel



-- UPDATE


type Msg 
    = Reset
    | Run
    | NextStep RunningAlias Time.Posix
    | NewList (List Int)
    | NewLookingFor (List Int) Int


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Reset ->
            resetModel

        Run ->
            case model.status of
                Idle lookingFor ->
                    let
                        current =
                            List.head model.previous
                        remaining =
                            List.drop 1 model.previous
                    in
                    (
                        { model
                        | previous = []
                        , status = runOrFound current remaining lookingFor
                        }
                    , Cmd.none
                    )

                Running data ->
                    let
                        previous 
                            = model.previous
                            ++ (data.current :: [])
                            ++ data.remaining
                        current =
                            List.head previous
                        remaining =
                            List.drop 1 previous
                    in
                    (
                        { model
                        | previous = []
                        , status = runOrFound current remaining data.lookingFor
                        }
                    , Cmd.none
                    )

                Found found ->
                    let
                        current =
                            List.head model.previous
                        remaining =
                            List.drop 1 model.previous
                    in
                    (
                        { model
                        | previous = []
                        , status = runOrFound current remaining found
                        }
                    , Cmd.none
                    )

        NextStep data newTime ->
            let
                previous =
                    model.previous ++ (data.current :: [])
                current =
                    List.head data.remaining
                remaining =
                    List.drop 1 data.remaining
            in
            (
                { model
                | previous = previous
                , status = runOrFound current remaining data.lookingFor
                }
            , Cmd.none
            )

        NewList list ->
            let
                sortedList =
                    List.sort list
                lookingFor =
                    Maybe.withDefault 10 (List.head sortedList)
            in
            ( Model [] (Idle lookingFor) (List.drop 1 sortedList)
            , Random.generate (NewLookingFor sortedList) lookingForGenerator
            )

        NewLookingFor list index ->
            if index == 0 then
                ( model
                , Cmd.none
                )
            else
                let
                    previous =
                        List.take (index - 1) list
                    lookingFor =
                        list
                            |> List.take index
                            |> List.reverse
                            |> List.head
                            |> Maybe.withDefault 10
                    others =
                        List.drop index list
                in
                ( Model previous (Idle lookingFor) others
                , Cmd.none
                )


runOrFound : Maybe Int -> List Int -> Int -> Status
runOrFound current remaining lookingFor =
    case current of
        Just number ->
            Running
                { current = number
                , remaining = remaining
                , lookingFor = lookingFor
                }

        Nothing ->
            Found lookingFor



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
        lookingForText =
            case model.status of
                Idle lookingFor ->
                    String.fromInt lookingFor
                Running data ->
                    String.fromInt data.lookingFor
                Found found ->
                    String.fromInt found

        foundText =
            case model.status of
                Found _ ->
                    String.fromInt (List.length model.previous)
                _ ->
                    "Unknown"
    in
        div [ class "algorithm" ]
            [ h1 [] [ text "Linear Search" ]
            , button [ onClick Run ] [ text "Run" ]
            , button [ onClick Reset ] [ text "Reset" ]
            , h2 [] [ text ("We are looking for: " ++ lookingForText)
                    , text (" | The index: " ++ foundText)
                    ]
            , viewSquares model
            ]


viewSquares : Model -> Html Msg
viewSquares model =
    let
        squares =
            case model.status of
                Idle lookingFor ->
                    List.map viewEmptySquare model.previous
                 ++ List.singleton (viewEmptySquare lookingFor)
                 ++ List.map viewEmptySquare model.others

                Running data ->
                    List.map viewEmptySquare model.previous
                 ++ List.singleton (viewCurrentSquare data.current)
                 ++ List.map viewEmptySquare data.remaining
                 ++ List.singleton (viewEmptySquare data.lookingFor)
                 ++ List.map viewEmptySquare model.others

                Found found ->
                    List.map viewEmptySquare model.previous
                 ++ List.singleton (viewFoundSquare found)
                 ++ List.map viewEmptySquare model.others
    in
    div [ class "squares" ] squares


viewEmptySquare : Int -> Html Msg
viewEmptySquare number =
    viewSquare "" (String.fromInt number)


viewCurrentSquare : Int -> Html Msg
viewCurrentSquare number =
    viewSquare "current" (String.fromInt number)


viewFoundSquare : Int -> Html Msg
viewFoundSquare number =
    viewSquare "found" (String.fromInt number)

viewSquare : String -> String -> Html Msg
viewSquare additionalClass value =
    div [ class "square", class additionalClass ] [ text value ]