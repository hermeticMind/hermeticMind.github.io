module View.Sigil exposing (SigilSort(..), view)

import Angle
import Arc2d
import Circle2d
import Data.Alphabet as Alphabet exposing (TwentySix)
import Data.Geometry as Geometry
import Data.Turtle as Turtle exposing (Turtle)
import Direction2d
import Geometry.Svg as Svg
import Html exposing (Html)
import LineSegment2d
import List.Extra as List
import Pixels
import Point2d exposing (Point2d)
import Quantity exposing (Quantity(..))
import StaticArray exposing (StaticArray)
import StaticArray.Index as Index exposing (Index)
import StaticArray.Length as Length exposing (Length)
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import Vector2d
import View.BinarySigil as BinarySigil
import View.GreekMagicSymbol as GreekMagicSymbol
import View.Sigil.Braid as Braid
import View.Sigil.MagicSquare as MagicSquare


type SigilSort
    = BraidSigil
    | MagicSquareSigil


type alias N =
    Alphabet.TwentySix


n : Length N
n =
    Alphabet.twentySix


view :
    { width : Float
    , height : Float
    , radius : Float
    , zoom : Float
    , asAlphabet : Char -> Index TwentySix
    , fillColor : String
    , strokeColor : String
    , withCircle : Bool
    , debugMode : Bool
    , withRunes : Bool
    , withText : Bool
    , withBorder : Bool
    , lineWidth : Float
    , strokeWidth : Float
    , sort : SigilSort
    }
    -> String
    -> Html msg
view { width, height, radius, fillColor, strokeColor, lineWidth, sort, withText, asAlphabet, withCircle, debugMode, strokeWidth, withBorder, zoom, withRunes } string =
    let
        pointSize =
            lineWidth / 2

        options =
            { width = width
            , height = height
            , radius = radius
            }

        list =
            string
                |> String.toList
                |> List.map asAlphabet

        runeSize =
            radius / 4

        uniqueList =
            list |> List.uniqueBy Index.toInt

        mostOccured =
            string
                |> String.toList
                |> List.gatherEquals
                |> List.map (Tuple.second >> List.length)
                |> List.maximum
                |> Maybe.withDefault 0

        outerRadius =
            radius + ((toFloat mostOccured + 2.5) * lineWidth)

        fnBorder =
            Turtle.andThen (Turtle.forwardBy pointSize)
                >> Turtle.andThen (Turtle.arcRightBy { angle = Angle.radians <| pi, radius = pointSize })
                >> Turtle.andThen (Turtle.forwardBy (pointSize * 4))
                >> Turtle.andThen (Turtle.arcLeftBy { angle = Angle.radians <| pi * 3 / 2, radius = pointSize })
                >> Turtle.andThen (Turtle.forwardBy (pointSize * 4))
                >> Turtle.andThen (Turtle.arcRightBy { angle = Angle.radians <| pi, radius = pointSize })

        border =
            if withRunes then
                [ Circle2d.atPoint (Point2d.pixels (width / 2) (height / 2))
                    (Pixels.pixels <| outerRadius)
                    |> Svg.circle2d
                        [ Attributes.fill <| "none"
                        , Attributes.stroke <| strokeColor
                        , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                        ]
                , Circle2d.atPoint (Point2d.pixels (width / 2) (height / 2))
                    (Pixels.pixels <|
                        outerRadius
                            + runeSize
                            + (toFloat 2 * lineWidth)
                    )
                    |> Svg.circle2d
                        [ Attributes.fill <| "none"
                        , Attributes.stroke <| strokeColor
                        , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                        ]
                ]

            else
                { position = Point2d.unsafe { x = width / 2, y = lineWidth * 2 }
                , direction = Direction2d.positiveX
                , lineFun =
                    \{ from, to } ->
                        let
                            segment =
                                LineSegment2d.from from to
                        in
                        [ segment
                            |> Svg.lineSegment2d
                                [ Attributes.stroke strokeColor
                                , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                                ]
                        ]
                , arcFun =
                    \{ around, by, from } ->
                        let
                            arc =
                                Arc2d.sweptAround around by from
                        in
                        [ arc
                            |> Svg.arc2d
                                [ Attributes.fill <| "none"
                                , Attributes.stroke <| strokeColor
                                , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                                ]
                        ]
                }
                    |> Turtle.forwardBy (width / 2 - lineWidth * 2 + pointSize / 2)
                    |> fnBorder
                    |> Turtle.andThen (Turtle.forwardBy (height - lineWidth * 4 + pointSize))
                    |> fnBorder
                    |> Turtle.andThen (Turtle.forwardBy (width - lineWidth * 4 + pointSize))
                    |> fnBorder
                    |> Turtle.andThen (Turtle.forwardBy (height - lineWidth * 4 + pointSize))
                    |> fnBorder
                    |> Turtle.andThen (Turtle.forwardBy (width / 2 - lineWidth * 2 + pointSize / 2))
                    |> Tuple.second
    in
    (case sort of
        BraidSigil ->
            Braid.view
                { width = width
                , height = height
                , radius = radius
                , asAlphabet = asAlphabet
                , fillColor = fillColor
                , strokeColor = strokeColor
                , debugMode = debugMode
                }
                string

        MagicSquareSigil ->
            MagicSquare.view
                { width = width
                , height = height
                , radius = radius
                , strokeWidth = strokeWidth
                , strokeColor = strokeColor
                , alphabet = asAlphabet
                , debugMode = debugMode
                }
                string
    )
        |> (if withCircle then
                List.append
                    (Circle2d.atPoint (Point2d.pixels (width / 2) (height / 2))
                        (Pixels.pixels <| radius)
                        |> Svg.circle2d
                            [ Attributes.fill <| "none"
                            , Attributes.stroke <| strokeColor
                            , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                            ]
                        |> List.singleton
                    )

            else
                identity
           )
        |> List.append
            (if withRunes then
                (string |> String.toList |> List.unique)
                    |> List.map
                        (\char ->
                            let
                                symbolLength =
                                    5

                                r =
                                    asAlphabet <| char

                                distance =
                                    outerRadius
                                        + (runeSize / 2)
                                        + (toFloat 1 * lineWidth)

                                point =
                                    Point2d.pixels (width / 2) (height / 2)
                                        |> Point2d.translateBy (Vector2d.pixels distance 0)
                                        |> Point2d.rotateAround (Point2d.pixels (width / 2) (height / 2))
                                            (Angle.radians <| (2 * pi / toFloat (Length.toInt n)) * (0.5 + toFloat (-7 + Index.toInt r)))

                                direction =
                                    (Angle.radians <| (2 * pi / toFloat (Length.toInt n)) * (0.5 + toFloat (Index.toInt r)))
                                        |> Direction2d.fromAngle
                                        |> Direction2d.rotateClockwise
                            in
                            GreekMagicSymbol.fromChar
                                { size = runeSize
                                , position = point |> Point2d.unwrap |> Point2d.unsafe
                                , direction = direction
                                , color = strokeColor
                                }
                                char
                                |> Maybe.withDefault []
                         {--{ value = Index.toInt <| r
                                , size = symbolLength
                                , color = "black"
                                , radius = 2
                                , strokeWidth = 1 / 2
                                , point = point
                                    
                                , direction = direction
                                }
                                |> BinarySigil.view--}
                        )
                    |> List.concat

             else
                []
            )
        |> List.append
            (if withText then
                [ Svg.text_
                    [ Attributes.fontFamily "Kaushan Script, serif"
                    , width / 2 |> String.fromFloat |> Attributes.x
                    , (if withRunes then
                        height - lineWidth * 3

                       else
                        height - lineWidth * 8
                      )
                        |> String.fromFloat
                        |> Attributes.y
                    , Attributes.textAnchor "middle"
                    , Attributes.alignmentBaseline "central"
                    , Attributes.fill strokeColor
                    ]
                    [ Svg.text string ]
                ]

             else
                []
            )
        |> List.append
            (if withBorder then
                border

             else
                []
            )
        |> Svg.svg
            [ Attributes.width <| (String.fromFloat <| zoom * width) ++ "px"
            , Attributes.height <| (String.fromFloat <| zoom * height) ++ "px"
            , Attributes.version <| "1.1"
            , Attributes.viewBox <|
                "0 0 "
                    ++ String.fromFloat width
                    ++ " "
                    ++ String.fromFloat height
            ]
