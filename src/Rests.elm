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

type alias ChoiceOfSets =
    List Int

type alias RestPeriod =
    {
        seconds: Int,
        label: String
    }

type alias ChoiceOfRests =
    List RestPeriod

type alias ButtonInfo =
    { colour: String
    , label: String
    , value: ChoiceOfSets
    , id: String
    , rest: ChoiceOfRests
    }

type Scene =
    SelectingWorkout ButtonPanel
    | SelectingSets ButtonInfo 
    | SelectingRests ButtonInfo String
    | Training ButtonInfo String String

-- [TimeConfig(60, "60sec"), TimeConfig(90, "90sec"), TimeConfig(120, "2mins"), TimeConfig(300, "5mins")]
restGroup1: ChoiceOfRests
restGroup1 = [RestPeriod 60 "60sec"
                , RestPeriod 90 "90sec"
                , RestPeriod 120 "2mins"
                , RestPeriod 300 "5mins"
                ]

workoutSelection: ButtonPanel
workoutSelection = [("Strength", ButtonInfo "pink" "S" [1, 3, 5, 6, 10] "Strength" restGroup1)
                    ,("Endurance", ButtonInfo "purple" "E" [2, 3] "Endurance" restGroup1)
                    ,("Hyper", ButtonInfo "red" "H" [3, 4] "Hyper" restGroup1)
                    ]
                    |> Dict.fromList

type alias Model = 
    Scene

type Msg =
    One ButtonInfo
    | SetsChosen ButtonInfo String
    | RestsChosen ButtonInfo String String
    
defaultButton: ButtonInfo
defaultButton = { colour = "black"
                ,  label = "Default"
                ,  value = []
                ,  id = "xx"
                ,  rest = []
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
            SelectingSets (workoutSelection 
                            |> Dict.get id.id
                            |> Maybe.withDefault defaultButton)
        SetsChosen button sets ->
            SelectingRests button sets
        RestsChosen button sets rest ->
            Training button sets rest
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
            -- below can be generated from the type
                , div [] [shape <| getButtonInfo "Strength" buttons] -- orange
                , div [] [shape <| getButtonInfo "Endurance" buttons]
                , div [] [shape <| getButtonInfo "Hyper" buttons]
            ]
        SelectingSets choice ->
            div [][
                div [][header]
                , div [] (shapeChoiceOfSets choice)
            ]
        SelectingRests choice sets -> 
            div [][
                div [][header]
                    , div [] (shapeChoiceOfRests choice sets)
                ]
        Training choice sets rest->
            div [][Html.text ("Training"++" "++sets++":"++rest)
                    , div [][
                        svg [ width "200"
                            , height "120"
                            , viewBox "0 0 200 120"
                    --        , onClick (RestsChosen button setsChosen (String.fromInt c.seconds))
                            ]
                            [ rect
                                [ x "10"
                                , y "10"
                                , width "180"
                                , height "90"
                                , rx "15"
                                , ry "15"
                                , fill "lightblue"
                                , id (sets++":"++rest)
                                ]
                                []
                            , text_
                                    [ x "45"
                                    , y "63"
                                    , fontSize "30px"
                                ]
                            [Html.text "Resting"
                            ]  
        
                       ]
                        , svg [ width "200"
                            , height "120"
                            , viewBox "0 0 200 120"
                    --        , onClick (RestsChosen button setsChosen (String.fromInt c.seconds))
                            ]
                            [ rect
                                [ x "10"
                                , y "10"
                                , width "180"
                                , height "90"
                                , rx "15"
                                , ry "15"
                                , fill "lightblue"
                                , id (sets++":"++rest)
                                ]
                                []
                            , text_
                                    [ x "45"
                                    , y "63"
                                    , fontSize "30px"
                                ]
                            [Html.text "Rested"
                            ]  
                            ]
        
                       ]
                    ]
            


shape: ButtonInfo->Html Msg
shape  info =
  svg
    [ width "120"
    , height "120"
    , viewBox "0 0 120 120"
    , onClick (One info)
    ]
    [ rect
        [ x "10"
        , y "10"
        , width "100"
        , height "100"
        , rx "15"
        , ry "15"
        , fill info.colour
        , id info.id
        ]
        []
      , text_
        [ x "43"
        , y "77"
        , fontSize "50px"
        ]
        [Html.text info.label]  
    ]

shapeChoiceOfSets: ButtonInfo->List (Html Msg)
shapeChoiceOfSets button = 
    let
        setList = button.value
    in
        
        List.map (\c -> 
        svg
            [ width "120"
            , height "120"
            , viewBox "0 0 120 120"
            , onClick (SetsChosen button (String.fromInt c))
            ]
            [ rect
                [ x "10"
                , y "10"
                , width "100"
                , height "100"
                , rx "15"
                , ry "15"
                , fill "grey"
                , id (String.fromInt c)
                ]
                []
            , text_
                    [ x "43"
                    , y "77"
                    , fontSize "50px"
                ]
            [Html.text (String.fromInt c)]  
    
            ]
        ) setList

shapeChoiceOfRests: ButtonInfo->String->List (Html Msg)
shapeChoiceOfRests button setsChosen = 
    let
        restsList = button.rest
    in
    List.map (\c -> 
    svg
        [ width "300"
        , height "120"
        , viewBox "0 0 300 120"
        , onClick (RestsChosen button setsChosen (String.fromInt c.seconds))
        ]
        [ rect
            [ x "10"
            , y "10"
            , width "200"
            , height "100"
            , rx "15"
            , ry "15"
            , fill "lightblue"
            , id c.label
            ]
            []
          , text_
                [ x "43"
                , y "77"
                , fontSize "50px"
            ]
        [Html.text c.label
        ]  
  
        ]
    ) restsList
