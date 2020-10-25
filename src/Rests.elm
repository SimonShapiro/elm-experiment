module Rests exposing (..)

import Browser
import Html exposing (Html, button, div, text, input, a, i, span)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Html.Events exposing (onInput)

import Svg exposing (..)
import Svg.Attributes exposing (..)

import Dict exposing(..)
import Maybe exposing (Maybe)

type alias ButtonPanel =
    Dict String ButtonInfo

type alias ButtonInfo =
    { colour: String
    , label: String
    , value: Int
    , id: String
    }

type Scene =
    SelectingWorkout ButtonPanel

workoutSelection: ButtonPanel
workoutSelection = [("Strength", ButtonInfo "pink" "Strength" 1 "Strength")
                    ,("Endurance", ButtonInfo "purple" "Endurance" 1 "Endurance")
                    ]
                    |> Dict.fromList

type alias Model = 
    Scene

type Msg =
    One ButtonInfo
    
defaultButton: ButtonInfo
defaultButton = { colour = "black"
                ,  label = "Default"
                ,  value = 0
                ,  id = "xx"
                }

init: Model
init = SelectingWorkout workoutSelection

main: Program () Model Msg
main =
  Browser.sandbox { init = init, update = update, view = view }
update: Msg->Model->Model
update msg model =
    case msg of
       One id ->
        let
            newPanel = (Dict.update id.id (Maybe.map (\b ->  { b | colour = "green"}   )) workoutSelection)
        in
            SelectingWorkout newPanel
    --        SelectingWorkout  (Dict.update id.id (Maybe.map (\b ->  { b | colour = "green"}   )) workoutSelection)
            
            --Maybe.withDefault "No user" defaultButton 
            
        -- "John Johnson"

    -- <svg width="400" height="110">
-- --  <rect width="300" height="100" style="fill:rgb(0,0,255);stroke-width:3;stroke:rgb(0,0,0)" />
-- -- </svg>
-- block: Html msg
-- block = svg [width "400", height "110"][rect [width "300" height "100" style "fill:rgb(0,0,255);stroke-width:3;stroke:rgb(0,0,0)" ][]]
getButtonInfo : String -> ButtonPanel -> ButtonInfo
getButtonInfo id buttonDict =
        buttonDict 
        |> Dict.get id
        |> Maybe.withDefault defaultButton
--    |> get 
--    |> Maybe.withDefault defaultButton.colour


header: Html msg
header = div [class "w3-bar w3-large w3-theme-d4"]
  [Html.a [href "#", class "w3-bar-item w3-button"][i [class "fa fa-bars"] []]
  , span [class "w3-bar-item"] [Html.text "RESTS & SETS"]
  ]
view: Model -> Html Msg
view model = 
    case model of
       SelectingWorkout buttons -> 
--        Debug.log ("There" ++ (getButtonInfo "Endurance" buttons).colour)
        div [][header
                , div [] [shape <| getButtonInfo "Strength" buttons] -- orange
                , div [] [shape <| getButtonInfo "Endurance" buttons]
            ]

shape: ButtonInfo->Html Msg
shape  info =
  svg
    [ width "120"
    , height "120"
    , viewBox "0 0 120 120"
    ]
    [ rect
        [ x "10"
        , y "10"
        , width "100"
        , height "100"
        , rx "15"
        , ry "15"
        , fill info.colour
        , onClick (One info)
        , id info.id
        ]
        []
    ]