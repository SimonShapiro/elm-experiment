port module Rests exposing (..)

import Browser
import Html exposing (Html, button, div, text, input, a, i, span, audio)
import Html.Attributes exposing (href, src, controls, autoplay, style)
import Html.Events exposing (onClick)
import Html.Events exposing (onInput)

import Svg exposing (..)
import Svg.Attributes exposing (..)

import Dict exposing(..)
import Maybe exposing (Maybe)

import Time

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

type alias SetsAndRests =
    {   targetSets: Int
        , targetRests: Int
        , currentSets: Int
        , currentRests: Int
    }

type Scene =
    SelectingWorkout ButtonPanel
    | SelectingSets ButtonInfo 
    | SelectingRests ButtonInfo String
    | Training ButtonInfo SetsAndRests Bool
    | Resting SetsAndRests

type alias ButtonConfig =
    {
        width: Int,
        height: Int,
        colour: String
    }

type alias Position =
    {
        x: Int,
        y: Int
    }

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


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 1000 Tick

type alias Model = 
    Scene

type Workout =
    Strength
    | Endurance
    | Hyper

type Msg =
    One Workout
    | SetsChosen ButtonInfo String
    | RestsChosen ButtonInfo String String
    | RestingStarted SetsAndRests
    | Tick Time.Posix
    | Rested SetsAndRests
    
defaultButton: ButtonInfo
defaultButton = { colour = "black"
                ,  label = "Default"
                ,  value = []
                ,  id = "xx"
                ,  rest = []
                }

init: ()->(Model, Cmd Msg)
init _ = (SelectingWorkout workoutSelection
        , Cmd.none)

main: Program () Model Msg
main =
  Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }  -- subscrptions
update: Msg->Model->(Model, Cmd Msg)
update msg model =
    case msg of
        One workout ->
            case workout of
                Strength ->
                    (SelectingSets (ButtonInfo "pink" "S" [1, 3, 5, 6, 10] "Strength" restGroup1), Cmd.none)
                Endurance ->
                    (SelectingSets (ButtonInfo "purple" "E" [2, 3] "Endurance" restGroup1), Cmd.none)
                Hyper ->
                    (SelectingSets (ButtonInfo "red" "H" [3, 4] "Hyper" restGroup1), Cmd.none)
        SetsChosen button sets ->
            (SelectingRests button sets, Cmd.none)
        RestsChosen button sets rest ->
            (Training button (SetsAndRests (sets |> String.toInt |> Maybe.withDefault 10)
                                                        (rest |> String.toInt |> Maybe.withDefault 20)
                                                        0 0) False, Cmd.none)
        RestingStarted setsAndRests->
        -- dummy sound play here
            (Resting setsAndRests, Cmd.batch [setSource "Door Bell-SoundBible.com-1986366504.mp3"
                                            , playMusic "Play"])
        Rested setsAndRests->
--            (model, playMusic "Play")
                -- sound alarm here
                let
                    newSets = setsAndRests.currentSets + 1 
                in
                    if newSets < setsAndRests.targetSets
                    then
                        (Training defaultButton {setsAndRests | currentRests = 0
                                                                , currentSets = newSets}  False, playMusic "play")
                    else
                        (SelectingWorkout workoutSelection, playMusic "play") -- sets finished perhaps a more dramatic sound
        Tick tick ->
            case model of
                Resting setsAndRests ->
                    let
                        newRests = (setsAndRests.currentRests + 1)
                    in
                        if newRests < setsAndRests.targetRests
                        then
                            Debug.log("Tick")
                            (Resting {setsAndRests | currentRests =  newRests}, Cmd.none)
                        else 
                            let
                                newSets = setsAndRests.currentSets + 1 
                            in
                                if newSets < setsAndRests.targetSets
                                then
                                        -- sound alarm here
                                    (Training defaultButton {setsAndRests | currentRests = 0
                                                                            , currentSets = newSets}  False, playMusic "play")
                                else
                                    (SelectingWorkout workoutSelection, playMusic "play") -- sets finished perhaps a more dramatic sound
                _ ->
                    (model, Cmd.none)


view: Model -> Html Msg
view model = 
    case model of
        SelectingWorkout buttons -> 
            div [][ div [][audio 
                [ id "beep"
        -- src can be a local file too.
                , src "Applause-SoundBible.com-151138312.mp3" -- since sets are finshed use another sound
                , controls False
                , autoplay True
                ] []
            ]
            , div [] [button [class "Sbutton", onClick (One Strength)][Html.text "Strength"]]
            , div [] [button [class "Ebutton", onClick (One Endurance)][Html.text "Endurance"]]
            , div [] [button [class "Hbutton", onClick (One Hyper)][Html.text "Hyper"]]
            ]
        SelectingSets choice ->
            div [](shapeChoiceOfSets choice)
        SelectingRests choice sets -> 
            div [](shapeChoiceOfRests choice sets)
        Training choice setsAndRest sound->
            div [][            div [][audio 
                [ id "beep"
        -- src can be a local file too.
                , src "Door Bell-SoundBible.com-1986366504.mp3"  -- "https://soundbible.com/mp3/Tyrannosaurus%20Rex%20Roar-SoundBible.com-807702404.mp3"

--                , src "https://soundbible.com/mp3/Tyrannosaurus%20Rex%20Roar-SoundBible.com-807702404.mp3"
                , controls False
                , autoplay sound
                ] []


            ]
            , div [class "bigFont"] [Html.text ("Sets"++" "++(String.fromInt <| setsAndRest.currentSets)++"/"++(String.fromInt <| setsAndRest.targetSets))]
            , div [][button [class "trainingButton", onClick (RestingStarted setsAndRest)]
                            [Html.text "Start Rest"]
                    ]
            ]  

        Resting setsAndRests->
            div [][div []
                [audio 
                [ id "beep"
        -- src can be a local file too.
                , src "Door Bell-SoundBible.com-1986366504.mp3"  -- "https://soundbible.com/mp3/Tyrannosaurus%20Rex%20Roar-SoundBible.com-807702404.mp3"

--                , src "https://soundbible.com/mp3/Tyrannosaurus%20Rex%20Roar-SoundBible.com-807702404.mp3"
                , controls False
                , autoplay False
                ] []
                ]
                , div [class "bigFont"] [Html.text (convertClock2TimeString setsAndRests.targetRests setsAndRests.currentRests)]
                ,div [][button [class "trainingButton", onClick (Rested setsAndRests)]
                            [Html.text "Back to work"   -- "Rested"
                            ]  
                        ]
                ]

shapeChoiceOfSets: ButtonInfo->List (Html Msg)
shapeChoiceOfSets buttonInfo = 
    let
        setList = buttonInfo.value
    in
        
        List.map (\c -> 
            button [class "setButton", onClick (SetsChosen buttonInfo (String.fromInt c))] 
            [Html.text (String.fromInt c)]  
        ) setList

shapeChoiceOfRests: ButtonInfo->String->List (Html Msg)
shapeChoiceOfRests buttonInfo setsChosen = 
    let
        restsList = buttonInfo.rest
    in
    List.map (\c -> 
            button [class "setButton", onClick (RestsChosen buttonInfo setsChosen (String.fromInt c.seconds))] 
            [Html.text c.label]  
    ) restsList

convertClock2TimeString: Int -> Int -> String
convertClock2TimeString rest tick =
    let
        secondsLeft = rest - tick
        minutes = secondsLeft // 60
        seconds = secondsLeft - (minutes * 60)
    in
        (String.padLeft 2 '0' (String.fromInt <| minutes))++":"++(String.padLeft 2 '0' (String.fromInt <| seconds))

-- PORTS

port playMusic : String -> Cmd msg


-- port stopMusic : String -> Cmd msg


port setSource : String -> Cmd msg