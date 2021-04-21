module Oracle exposing (..)

import Angle
import Browser
import Data.Alphabet as Alphabet
import Data.Card as Card exposing (Card)
import Dict exposing (Dict)
import Direction2d
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Geometry.Svg as Svg
import Html exposing (Html)
import Point2d
import Random
import Random.List as Random
import Svg
import Svg.Attributes as SvgAttributes
import Time
import Vector2d
import View.BinarySigil as BinarySigil
import View.BraidSigil as BraidSigil
import View.Card as Card
import View.Page as Page


type alias Model =
    { offsetAngle : Float
    , cards : List Card
    , flipped : Dict Int ()
    , question : String
    }


type Msg
    = TimePassed Float
    | PressedGetCards
    | GotCards (List Card)
    | FlipCard Int
    | UpdatedQuestion String
    | Reset


init : () -> ( Model, Cmd Msg )
init () =
    ( { offsetAngle = 0
      , cards = []
      , flipped = Dict.empty
      , question = ""
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    let
        ms =
            100
    in
    Time.every ms (always (TimePassed ms))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimePassed _ ->
            ( { model
                | offsetAngle = model.offsetAngle + 0.005
              }
            , Cmd.none
            )

        PressedGetCards ->
            ( model
            , Random.generate GotCards
                (Random.choices 3 Card.asList
                    |> Random.map Tuple.first
                )
            )

        GotCards cards ->
            ( { model | cards = cards }, Cmd.none )

        FlipCard i ->
            ( { model
                | flipped =
                    model.flipped
                        |> Dict.update i (always <| Just ())
              }
            , Cmd.none
            )

        UpdatedQuestion string ->
            ( { model
                | question = string
              }
            , Cmd.none
            )

        Reset ->
            ( { model
                | cards = []
                , flipped = Dict.empty
                , question = ""
              }
            , Cmd.none
            )


viewCircle : { offset : Float, n : Int, size : Float, radius : Float, strokeWidth : Float } -> Html Msg
viewCircle { offset, n, size, radius, strokeWidth } =
    List.range 0 (2 ^ n - 1)
        |> List.concatMap
            (\r ->
                let
                    angle =
                        Angle.radians <| (2 * pi / toFloat (2 ^ n)) * (0.5 + toFloat r) + offset
                in
                { value = r
                , size = n
                , color = "black"
                , radius = strokeWidth * 2
                , strokeWidth = strokeWidth
                , point =
                    Point2d.pixels (size / 2) (size / 2)
                        |> Point2d.translateBy (Vector2d.pixels radius 0)
                        |> Point2d.rotateAround (Point2d.pixels (size / 2) (size / 2))
                            angle
                , direction =
                    angle
                        |> Direction2d.fromAngle
                        |> Direction2d.rotateClockwise

                --Direction2d.positiveX --
                }
                    |> BinarySigil.view
            )
        |> Svg.svg
            [ SvgAttributes.width <| (String.fromFloat <| size) ++ "px"
            , SvgAttributes.height <| (String.fromFloat <| size) ++ "px"
            , SvgAttributes.version <| "1.1"
            , SvgAttributes.viewBox <|
                "0 0 "
                    ++ String.fromFloat size
                    ++ " "
                    ++ String.fromFloat size
            ]


view : Model -> Html Msg
view model =
    let
        size =
            800

        radius =
            350

        diversion =
            0.5

        cardZoom =
            0.3
    in
    (case model.cards of
        [] ->
            [ "Frage stellen"
                |> Element.text
                |> Element.el [ Font.color <| Element.rgb255 255 255 255 ]
            , Input.multiline
                [ Element.width <| Element.px <| round <| size / 2
                , Background.color <| Element.rgba255 0 0 0 0
                , Font.color <| Element.rgb255 255 255 255
                ]
                { onChange = UpdatedQuestion
                , text = model.question
                , placeholder = Nothing
                , label = Input.labelHidden "Deine Frage"
                , spellcheck = False
                }
            , Input.button
                [ Font.size 32
                , Border.width <| 0
                , Element.centerX
                , Element.centerY
                , Font.color <| Element.rgb255 255 255 255
                , Font.shadow
                    { offset = ( 0, 0 )
                    , blur = 32
                    , color = Element.rgb255 0 0 255
                    }
                ]
                { onPress = PressedGetCards |> Just
                , label =
                    "Orakel befragen"
                        |> Element.text
                }
            ]
                |> Element.column [ Element.centerX, Element.spacing 4 ]

        list ->
            [ [ Input.button
                    [ Font.size 16
                    , Border.width <| 0
                    , Font.color <| Element.rgb255 255 255 255
                    , Font.shadow
                        { offset = ( 0, 0 )
                        , blur = 32
                        , color = Element.rgb255 0 0 255
                        }
                    , Element.padding 16
                    ]
                    { onPress = Reset |> Just
                    , label =
                        "Wiederholen"
                            |> Element.text
                    }
              , if
                    model.flipped
                        |> Dict.size
                        |> (==) 3
                then
                    Element.link
                        [ Font.size 16
                        , Border.width <| 0
                        , Font.color <| Element.rgb255 255 255 255
                        , Font.shadow
                            { offset = ( 0, 0 )
                            , blur = 32
                            , color = Element.rgb255 0 0 255
                            }
                        , Element.padding 16
                        ]
                        { url = "https://www.etsy.com/HermeticMind/listing/971853626"
                        , label =
                            "Karten auf Etsy kaufen"
                                |> Element.text
                        }

                else
                    Element.none
              ]
                |> Element.row [ Element.spaceEvenly, Element.width <| Element.fill ]
            , list
                |> List.indexedMap
                    (\i card ->
                        Input.button
                            [ Element.height <| Element.px <| round <| cardZoom * Card.height + 2
                            , Element.width <| Element.px <| round <| cardZoom * Card.width + 2
                            , Border.width 1
                            , Border.shadow
                                { offset = ( 0, 0 )
                                , size = 4
                                , blur = 32
                                , color = Element.rgb255 0 0 127
                                }
                            ]
                            { onPress = Just <| FlipCard i
                            , label =
                                (case model.flipped |> Dict.get i of
                                    Just () ->
                                        card

                                    Nothing ->
                                        Card.back
                                )
                                    |> Card.view
                                    |> Svg.svg
                                        [ SvgAttributes.width <| (String.fromInt <| round <| cardZoom * Card.width) ++ "px"
                                        , SvgAttributes.height <| (String.fromInt <| round <| cardZoom * Card.height) ++ "px"
                                        , SvgAttributes.version <| "1.1"
                                        , SvgAttributes.viewBox <|
                                            "0 0 "
                                                ++ String.fromFloat Card.width
                                                ++ " "
                                                ++ String.fromFloat Card.height
                                        ]
                                    |> Element.html
                            }
                    )
                |> Element.row
                    [ Element.spacing 16
                    ]
            , Element.none |> Element.el []
            ]
                |> Element.column
                    [ Element.centerX
                    , Element.spaceEvenly
                    ]
    )
        |> Element.el
            [ Element.centerX
            , Element.centerY
            , Element.width <| Element.px 850
            ]
        |> Element.el
            [ Element.width <| Element.fill
            , Element.height <| Element.fill
            , model.question
                |> String.right 50
                |> BraidSigil.view
                    { width = size
                    , height = size
                    , radius = radius * 0.8
                    , zoom = 1
                    , asAlphabet = Alphabet.german
                    , withCircle = True
                    , debugMode = False
                    , withRunes = False
                    , withText = False
                    , withBorder = False
                    , fillColor = "black"
                    , strokeColor = "none"
                    }
                |> Element.html
                |> Element.el
                    [ viewCircle
                        { offset = model.offsetAngle
                        , n = 4
                        , size = size
                        , radius = radius
                        , strokeWidth = 2
                        }
                        |> Element.html
                        |> Element.el
                            [ viewCircle
                                { offset = model.offsetAngle * (1 + diversion)
                                , n = 3
                                , size = size
                                , radius = radius
                                , strokeWidth = 2
                                }
                                |> Element.html
                                |> Element.el
                                    [ viewCircle
                                        { offset = -model.offsetAngle * (1 + diversion * 2)
                                        , n = 2
                                        , size = size
                                        , radius = radius
                                        , strokeWidth = 2
                                        }
                                        |> Element.html
                                        |> Element.el
                                            [ viewCircle
                                                { offset = -model.offsetAngle * (1 + diversion * 3)
                                                , n = 1
                                                , size = size
                                                , radius = radius
                                                , strokeWidth = 2
                                                }
                                                |> Element.html
                                                |> Element.inFront
                                            , Element.centerX
                                            , Element.centerY
                                            ]
                                        |> Element.inFront
                                    , Element.centerX
                                    , Element.centerY
                                    ]
                                |> Element.inFront
                            , Element.centerX
                            , Element.centerY
                            ]
                        |> Element.inFront
                    , Element.centerX
                    , Element.centerY
                    ]
                |> Element.behindContent
            ]
        |> Page.view
            [ Background.color <|
                Element.rgb255 ((round <| sin (model.offsetAngle * 25) * 32) + 48)
                    0
                    ((round <| sin (model.offsetAngle * 30) * 8) + 48)
            ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view =
            view
        , update = update
        , subscriptions = subscriptions
        }
