import Browser
import Html exposing (Html, div, text, h1, button, h2)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Random
import Array exposing (Array)
import Time


main = 
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


arrayGenerator : Random.Generator (List Int)
arrayGenerator = Random.list 10 (Random.int 10 99)


lookingForGenerator : Random.Generator Int
lookingForGenerator = Random.int 0 9



--MODEL


type Status
    = Idle
    | Running Int
    | Found Int
    | NotFound
    | Error


type alias Model =
    { array : Array Int
    , lookingFor : Int
    , status : Status
    }


init : () -> (Model, Cmd Msg)
init _ = 
    ( Model Array.empty 0 Idle
    , Random.generate NewArray arrayGenerator
    )



-- UPDATE


type Msg 
    = Reset
    | Run
    | NextStep Time.Posix
    | NewArray (List Int)
    | NewLookingFor Int


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Reset ->
            ( { model | status = Idle }
            , Random.generate NewArray arrayGenerator
            )

        Run ->
            ( { model | status = Running 0 }
            , Cmd.none
            )

        NextStep newTime ->
            case model.status of
                Running index ->
                    updateRunningModel index model
                _ -> 
                    ( { model | status = Error }
                    , Cmd.none
                    )

        NewArray list ->
            ( { model | array = Array.fromList (List.sort list) }
            , Random.generate NewLookingFor lookingForGenerator
            )

        NewLookingFor index ->
            case Array.get index model.array of
                Just value ->
                    ( { model | lookingFor = value }
                    , Cmd.none
                    )
                Nothing ->
                    ( { model | status = Error }
                    , Cmd.none
                    )


updateRunningModel : Int -> Model -> (Model, Cmd Msg)
updateRunningModel index model =
    case Array.get index model.array of
        Just value ->
            let
                status i =
                    if value == model.lookingFor then
                        Found i
                    else
                        Running (i + 1)
            in
            ( { model | status = status index }
            , Cmd.none
            )

        Nothing ->
            ( { model | status = NotFound }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.status of
        Running current ->
            Time.every 1000 NextStep
        _ ->
            Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        lookingForText =
            case model.status of
                Error ->
                    "We got some error here :("
                _ ->
                    "We are looking for: " ++ String.fromInt model.lookingFor

        foundText =
            case model.status of
                Found index ->
                    " | The index: " ++ String.fromInt index
                NotFound ->
                    " | Strange, but we've found nothing :o"
                _ ->
                    ""
    in
        div [ class "algorithm" ]
            [ h1 [] [ text "Linear Search" ]
            , button [ onClick Run ] [ text "Run" ]
            , button [ onClick Reset ] [ text "Reset" ]
            , h2 [] [ text (lookingForText ++ foundText) ]
            , viewSquares model
            ]


viewSquares : Model -> Html Msg
viewSquares model = 
    div [ class "squares" ]
        ( model.array
          |> Array.indexedMap (viewSquare model)
          |> Array.toList
        )


viewSquare : Model -> Int -> Int -> Html Msg
viewSquare model index value =
    let
        additionalClass =
            case model.status of
                Running current ->
                    hasAdditionalClass current "current"

                Found found ->
                    hasAdditionalClass found "found"

                _ ->
                    ""

        hasAdditionalClass selectedIndex name =
            if index == selectedIndex then
                name
            else
                ""

        stringValue = String.fromInt value
    in
    div [ class "square", class additionalClass ] [ text stringValue ]