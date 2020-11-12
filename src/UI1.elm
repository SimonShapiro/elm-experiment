module UI1 exposing (..)

import Browser exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Element.Events exposing (onClick)

type Model = 
    ShowingServices
    | ShowingContent

type Msg =
    ServicesClicked

handleService: Msg
handleService =
    Debug.log("Services Clicked")
    ServicesClicked

logo : Element msg
logo =
    el
        [ width <| px 80
        , height <| px 40
        , Border.width 2
        , Border.rounded 6
        , Border.color <| rgb255 0xc0 0xc0 0xc0

        ]
        none


header : Element Msg
header =
    row [ width fill, padding 20, spacing 20 ]
        [ logo
        , el [ alignRight
             , onClick  handleService
             ] <| text "Services"
        , el [ alignRight ] <| text "About"
        , el [ alignRight ] <| text "Contact"
        ]


content : Model -> Element msg
content model =
    List.range 2 16
        |> List.map
            (\i ->
                el [ centerX, Font.size 64, Font.color <| rgb255 (i * 20) (i * 20) (i * 20) ] <|
                    case model of
                        ShowingServices ->
                            text "Scrollable services"
                        ShowingContent -> 
                            text "Scrollable content"
            )
        |> column [ width fill, padding 50 ]

footer : Element msg
footer =
    row
        [ width fill
        , padding 10
        , Background.color <| rgb255 0xFF 0xFC 0xF6
        , Border.widthEach { top = 2, bottom = 0, left = 0, right = 0 }
        , Border.color <| rgb255 0xc0 0xc0 0xc0
        ]
        [ logo
        , column [ alignRight, spacing 10 ]
            [ el [ alignRight ] <| text "Services"
            , el [ alignRight ] <| text "About"
            , el [ alignRight ] <| text "Contact"
            ]
        ]

init:  flags -> (Model, Cmd msg) 
init flags = 
    (ShowingContent, Cmd.none)

update: Msg -> Model -> (Model, Cmd msg)
update msg model =
    case msg of
        ServicesClicked -> 
            (ShowingServices, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

view : Model -> Html Msg
view model =
    layout [ width fill, height fill ] <|
        column [ width fill ]
            [ header 
            , content model
            , footer
            ]

main: Program () Model Msg
main =
  Browser.element { init = init, update = update, view = view, subscriptions = subscriptions } --subscriptions } 