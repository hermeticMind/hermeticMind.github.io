module View.Interactive exposing (view)

import Binary
import Css
import Data.Alphabet as Alphabet
import Data.Card as Card exposing (Card(..))
import Direction2d
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Markdown.Parser as Parser
import Parser exposing ((|.), (|=))
import Point2d
import Result.Extra as Result
import Svg
import Svg.Attributes as SvgAttributes
import View.AttributeDiagram as AttributeDiagram
import View.BinarySigil as BinarySigil
import View.Card as Card
import View.GreekMagicSymbol as GreekMagicSymbol
import View.Sigil as Sigil exposing (SigilSort(..))


view : Bool -> String -> Maybe String -> List (Html msg) -> Html msg
view isGerman name maybeValue content =
    case name of
        "greekMagicSymbols" ->
            let
                size =
                    16

                zoom =
                    1
            in
            Alphabet.asList
                |> List.filterMap
                    (GreekMagicSymbol.fromChar
                        { size = size
                        , position = Point2d.unsafe { x = size / 2, y = size / 2 }
                        , direction = Direction2d.positiveX
                        , color = "black"
                        }
                        >> Maybe.map
                            (Svg.svg
                                [ SvgAttributes.width <| (String.fromFloat <| zoom * size) ++ "px"
                                , SvgAttributes.height <| (String.fromFloat <| zoom * size) ++ "px"
                                , SvgAttributes.version <| "1.1"
                                , SvgAttributes.viewBox <|
                                    "0 0 "
                                        ++ String.fromFloat size
                                        ++ " "
                                        ++ String.fromFloat size
                                ]
                                >> Html.fromUnstyled
                                >> List.singleton
                                >> Html.span
                                    [ Attributes.css
                                        [ Css.padding2 (Css.px 0) (Css.px 1)
                                        ]
                                    ]
                            )
                    )
                |> Html.div
                    [ Attributes.css
                        [ Css.displayFlex
                        , Css.justifyContent Css.center
                        , Css.margin4 (Css.px 14) (Css.px 0) (Css.px 0) (Css.px 0)
                        ]
                    ]

        "BinarySigil" ->
            let
                width : Float
                width =
                    55

                height : Float
                height =
                    55

                binary =
                    maybeValue
                        |> Maybe.withDefault "0"
                        |> String.toList
                        |> List.map (String.fromChar >> String.toInt >> Maybe.withDefault 0)

                value =
                    binary
                        |> Binary.fromIntegers
                        |> Binary.toDecimal
            in
            BinarySigil.view
                { point =
                    Point2d.unsafe
                        { x = width / 2, y = height / 2 }
                , value = value
                , size = binary |> List.length
                , color = "black"
                , radius = 4
                , strokeWidth = 2
                , direction = Direction2d.positiveX
                }
                |> Svg.svg
                    [ SvgAttributes.width <| (String.fromFloat <| width) ++ "px"
                    , SvgAttributes.height <| (String.fromFloat <| height) ++ "px"
                    , SvgAttributes.version <| "1.1"
                    , SvgAttributes.viewBox <|
                        "0 0 "
                            ++ String.fromFloat width
                            ++ " "
                            ++ String.fromFloat height
                    ]
                |> Html.fromUnstyled

        "MagicSquareSigil" ->
            maybeValue
                |> Maybe.withDefault "Hermetic Mind"
                |> Sigil.view
                    { width = 256
                    , height = 256
                    , radius = 100
                    , zoom = 1
                    , lineWidth = 4
                    , strokeWidth = 1
                    , withBorder = False
                    , withText = True
                    , withRunes = False
                    , withCircle = True
                    , asAlphabet = Alphabet.english
                    , sort = MagicSquareSigil
                    , debugMode = False
                    , fillColor = "white"
                    , strokeColor = "black"
                    }
                |> Html.fromUnstyled
                |> List.singleton
                |> Html.div
                    [ Attributes.css
                        [ Css.displayFlex
                        , Css.justifyContent Css.center
                        , Css.margin4 (Css.px 8) (Css.px 0) (Css.px 0) (Css.px 0)
                        ]
                    ]

        "BraidSigil" ->
            maybeValue
                |> Maybe.withDefault "Hermetic Mind"
                |> Sigil.view
                    { width = 256
                    , height = 256
                    , radius = 100
                    , zoom = 1
                    , asAlphabet = Alphabet.english
                    , withCircle = True
                    , debugMode = False
                    , withRunes = False
                    , withText = False
                    , withBorder = False
                    , fillColor = "white"
                    , strokeColor = "black"
                    , sort = BraidSigil
                    , lineWidth = 4
                    , strokeWidth = 1
                    }
                |> Html.fromUnstyled
                |> List.singleton
                |> Html.div
                    [ Attributes.css
                        [ Css.displayFlex
                        , Css.justifyContent Css.center
                        , Css.margin4 (Css.px 8) (Css.px 0) (Css.px 0) (Css.px 0)
                        ]
                    ]

        "Oracle" ->
            let
                cardZoom =
                    0.2
            in
            maybeValue
                |> Maybe.withDefault "Back"
                |> Parser.run
                    Card.parser
                |> Result.withDefault Back
                |> Card.view isGerman
                |> Svg.svg
                    [ SvgAttributes.width <| (String.fromInt <| round <| cardZoom * Card.defaultConfig.width) ++ "px"
                    , SvgAttributes.height <| (String.fromInt <| round <| cardZoom * Card.defaultConfig.height) ++ "px"
                    , SvgAttributes.version <| "1.1"
                    , SvgAttributes.viewBox <|
                        "0 0 "
                            ++ String.fromFloat Card.defaultConfig.width
                            ++ " "
                            ++ String.fromFloat Card.defaultConfig.height
                    ]
                |> Html.fromUnstyled
                |> List.singleton
                |> Html.div
                    [ Attributes.css
                        [ Css.border3 (Css.px 1) Css.solid (Css.rgb 0 0 0)
                        , Css.width <| Css.px <| (toFloat <| round <| cardZoom * Card.defaultConfig.width)
                        , Css.height <| Css.px <| (toFloat <| round <| cardZoom * Card.defaultConfig.height)
                        ]
                    ]

        "Attributes" ->
            let
                size =
                    600
            in
            maybeValue
                |> Maybe.withDefault ""
                |> String.toList
                |> AttributeDiagram.view isGerman size
                |> Svg.svg
                    [ SvgAttributes.width <| (String.fromInt <| round <| size) ++ "px"
                    , SvgAttributes.height <| (String.fromInt <| round <| size) ++ "px"
                    , SvgAttributes.version <| "1.1"
                    , SvgAttributes.viewBox <|
                        "0 0 "
                            ++ String.fromFloat size
                            ++ " "
                            ++ String.fromFloat size
                    ]
                |> Html.fromUnstyled
                |> List.singleton
                |> Html.div
                    [ Attributes.css
                        [ Css.width <| Css.px <| (toFloat <| round <| size)
                        , Css.height <| Css.px <| (toFloat <| round <| size)
                        , Css.margin Css.auto
                        ]
                    ]

        _ ->
            Html.text "Content not found"
