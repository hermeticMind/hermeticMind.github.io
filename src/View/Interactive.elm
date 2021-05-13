module View.Interactive exposing (view)

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
import View.BinarySigil as BinarySigil
import View.BraidSigil as BraidSigil
import View.Card as Card
import View.GreekMagicSymbol as GreekMagicSymbol
import View.MagicSquareSigil as MagicSquareSigil


view : String -> Maybe String -> List (Html msg) -> Html msg
view name maybeValue content =
    (case name of
        "greekMagicSymbols" ->
            Alphabet.asList
                |> List.filterMap
                    (GreekMagicSymbol.fromChar 16
                        >> Maybe.map
                            (Html.fromUnstyled
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
                |> List.singleton

        "BinarySigil" ->
            let
                width : Float
                width =
                    40

                height : Float
                height =
                    55

                --64
            in
            List.range 0 (2 ^ 2 - 1)
                |> List.map
                    (\value ->
                        BinarySigil.view
                            { point =
                                Point2d.unsafe
                                    { x = width / 2, y = height / 2 }
                            , value = value
                            , size = 2
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
                    )
                |> Html.div
                    [ Attributes.css
                        [ Css.displayFlex
                        , Css.justifyContent Css.center
                        ]
                    ]
                |> List.singleton

        "MagicSquareSigil" ->
            maybeValue
                |> Maybe.withDefault "Hermetic Mind"
                |> MagicSquareSigil.view
                    { size = 128
                    , zoom = 1
                    , strokeWidth = 2
                    , withBorder = False
                    , withText = True
                    , alphabet = Alphabet.english
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
                |> List.singleton

        "BraidSigil" ->
            maybeValue
                |> Maybe.withDefault "Hermetic Mind"
                |> BraidSigil.view
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
                |> List.singleton

        "Oracle" ->
            let
                cardZoom =
                    0.2
            in
            maybeValue
                |> Maybe.withDefault "(back,back,back)"
                |> Parser.run
                    (Parser.succeed (\c1 c2 c3 -> [ c1, c2, c3 ])
                        |. Parser.symbol "("
                        |= Card.parser
                        |. Parser.symbol ","
                        |= Card.parser
                        |. Parser.symbol ","
                        |= Card.parser
                        |. Parser.symbol ")"
                    )
                |> Result.withDefault [ Back, Back, Back ]
                |> List.map
                    (Card.view False
                        >> Svg.svg
                            [ SvgAttributes.width <| (String.fromInt <| round <| cardZoom * Card.width) ++ "px"
                            , SvgAttributes.height <| (String.fromInt <| round <| cardZoom * Card.height) ++ "px"
                            , SvgAttributes.version <| "1.1"
                            , SvgAttributes.viewBox <|
                                "0 0 "
                                    ++ String.fromFloat Card.width
                                    ++ " "
                                    ++ String.fromFloat Card.height
                            ]
                        >> Html.fromUnstyled
                        >> List.singleton
                        >> Html.div
                            [ Attributes.css
                                [ Css.border3 (Css.px 1) Css.solid (Css.rgb 0 0 0)
                                , Css.width <| Css.px <| (toFloat <| round <| cardZoom * Card.width)
                                , Css.height <| Css.px <| (toFloat <| round <| cardZoom * Card.height)
                                ]
                            ]
                    )
                |> Html.div
                    [ Attributes.css
                        [ Css.displayFlex
                        , Css.justifyContent Css.spaceBetween
                        ]
                    ]
                |> List.singleton

        _ ->
            Html.text "Content not found"
                |> List.singleton
    )
        |> (\list ->
                list
                    ++ (content
                            |> Html.div
                                [ Attributes.css
                                    [ Css.displayFlex
                                    , Css.justifyContent Css.center
                                    ]
                                ]
                            |> List.singleton
                       )
           )
