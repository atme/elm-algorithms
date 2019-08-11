module Skeleton exposing (Tab(..), sidebar, skeleton)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)



-- SKELETON


skeleton : String -> Tab -> List (Html Never) -> Program () () Never
skeleton title tab content =
    Browser.document
        { init = \_ -> ( (), Cmd.none )
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        , view =
            \_ ->
                { title = title
                , body = sidebar tab :: content
                }
        }



-- SIDEBAR


type Tab
    = SearchAlgorithms
    | SortingAlgorithms
    | DataStructures


sidebar : Tab -> Html msg
sidebar tab =
    div [ class "sidebar" ]
        [ ul [] <|
            List.map (viewTab tab) <|
                [ TabInfo SearchAlgorithms "Search Algorithms" "index.html"
                , TabInfo SortingAlgorithms "Sorting Algorithms" "sort.html"
                , TabInfo DataStructures "Data Structures" "datastructure.html"
                ]
        ]


viewTab : Tab -> TabInfo -> Html msg
viewTab currentTab info =
    li []
        [ if currentTab == info.tab then
            text (info.name ++ " ->")

          else
            a [ href info.link ] [ text info.name ]
        ]


type alias TabInfo =
    { tab : Tab
    , name : String
    , link : String
    }
