module MagicCircle exposing (..)

import Angle
import Circle2d
import Direction2d
import Geometry.Svg as Svg
import Html exposing (Html)
import LineSegment2d
import Pixels
import Point2d
import Svg
import Svg.Attributes as Attributes
import Vector2d
import View.BinarySigil as Sigil


zoom : Float
zoom =
    6


size =
    100


ringWidth =
    5


main : Html msg
main =
    [ Circle2d.atPoint (Point2d.pixels (size / 2) (size / 2))
        (Pixels.pixels <| ringWidth)
        |> Svg.circle2d
            [ Attributes.fill <| "none"
            , Attributes.stroke <| "black"
            , Attributes.strokeWidth <| String.fromFloat <| 0.2
            ]
    ]
        {--++ ({ value = 0
            , size = 0
            , color = "black"
            , radius = View.relative <| 1 / 2
            , strokeWidth = View.relative <| 1 / 8
            , point = Point2d.pixels (size / 2) (size / 2)
            }
                |> Sigil.view
           )--}
        ++ (List.range 1 4
                |> List.concatMap
                    (\n ->
                        (Circle2d.atPoint (Point2d.pixels (size / 2) (size / 2))
                            (Pixels.pixels <| ringWidth * (1 + 2 * toFloat n))
                            |> Svg.circle2d
                                [ Attributes.fill <| "none"
                                , Attributes.stroke <| "black"
                                , Attributes.strokeWidth <| String.fromFloat <| 1
                                ]
                        )
                            :: (List.range 0 ((2 ^ n) - 1)
                                    |> List.concatMap
                                        (\r ->
                                            (Point2d.pixels (size / 2) (size / 2)
                                                |> Point2d.translateBy (Vector2d.pixels (ringWidth * (1 + 2 * toFloat n)) 0)
                                                |> LineSegment2d.from
                                                    (Point2d.pixels (size / 2) (size / 2)
                                                        |> Point2d.translateBy (Vector2d.pixels (ringWidth * (1 + 2 * (toFloat n - 1))) 0)
                                                    )
                                                |> LineSegment2d.rotateAround (Point2d.pixels (size / 2) (size / 2))
                                                    (Angle.radians <| (2 * pi / toFloat (2 ^ n)) * toFloat r)
                                                |> Svg.lineSegment2d
                                                    [ Attributes.stroke "black"
                                                    , Attributes.strokeWidth <| String.fromFloat <| 0.2
                                                    , Attributes.fill "none"
                                                    ]
                                            )
                                                :: (let
                                                        angle =
                                                            Angle.radians <| (2 * pi / toFloat (2 ^ n)) * (0.5 + toFloat r)
                                                    in
                                                    { value = r
                                                    , size = n
                                                    , color = "black"
                                                    , radius = 1 / 2
                                                    , strokeWidth = 1 / 4 --1 / 8
                                                    , point =
                                                        Point2d.pixels (size / 2) (size / 2)
                                                            |> Point2d.translateBy (Vector2d.pixels (1.5 * ringWidth + ringWidth * (2 * toFloat (n - 1))) 0)
                                                            |> Point2d.rotateAround (Point2d.pixels (size / 2) (size / 2))
                                                                angle
                                                    , direction =
                                                        angle
                                                            |> Direction2d.fromAngle
                                                            |> Direction2d.rotateClockwise

                                                    --Direction2d.positiveX --
                                                    }
                                                        |> Sigil.view
                                                   )
                                        )
                               )
                    )
           )
        |> Svg.svg
            [ Attributes.width <| (String.fromFloat <| zoom * size) ++ "px"
            , Attributes.height <| (String.fromFloat <| zoom * size) ++ "px"
            , Attributes.version <| "1.1"
            , Attributes.viewBox <|
                "0 0 "
                    ++ String.fromFloat size
                    ++ " "
                    ++ String.fromFloat size
            ]