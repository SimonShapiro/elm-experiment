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

type alias Model =
    Dict String ButtonInfo
    

type alias ButtonInfo =
    { colour: String
    , label: String
    , value: Int
    , id: String
    }


type Msg =
    One String
    
defaultButton: ButtonInfo
defaultButton = { colour = "black"
                ,  label = "Default"
                ,  value = 0
                ,  id = "xx"
                }

init: Model
init = [("b1", { colour = "orange"
                    , label = "Button 1"
                    , value = 1
                    , id = "b1"
                })
        ,("b2", { colour = "purple"
                    , label = "Button 2"
                    , value = 1
                    , id = "b2"
                })] 
        |> Dict.fromList

main: Program () Model Msg
main =
  Browser.sandbox { init = init, update = update, view = view }
update: Msg->Model->Model
update msg model =
    case msg of
       One id ->
            Dict.update id (Maybe.map (\b ->  { b | colour = "green"}   )) model 
            
            --Maybe.withDefault "No user" defaultButton 
            
        -- "John Johnson"

    -- <svg width="400" height="110">
-- --  <rect width="300" height="100" style="fill:rgb(0,0,255);stroke-width:3;stroke:rgb(0,0,0)" />
-- -- </svg>
-- block: Html msg
-- block = svg [width "400", height "110"][rect [width "300" height "100" style "fill:rgb(0,0,255);stroke-width:3;stroke:rgb(0,0,0)" ][]]
getButtonColour : String -> Model -> ButtonInfo
getButtonColour id buttonDict =
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
    div [][header
            , shape <| getButtonColour "b2" model
            , shape <| getButtonColour "b1" model
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
        , onClick (One info.id)
        , id info.id
        ]
        []
    ]