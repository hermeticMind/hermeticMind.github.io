module View.AttributeDiagram exposing (view)

import Angle
import Array
import Data.Attribute as Attribute
import Geometry.Svg as Svg
import LineSegment2d exposing (LineSegment2d)
import Point2d
import Polygon2d
import Polyline2d
import Svg exposing (Svg)
import Svg.Attributes as Attributes


view : Bool -> Int -> List Char -> List (Svg msg)
view isGerman size attr =
    let
        radius =
            toFloat size * 3 / 8

        attributeAmount =
            4

        n =
            2 * attributeAmount

        strokeWidth =
            1

        array =
            attr
                |> Array.fromList

        diagramRadius =
            radius * 1

        textRadius =
            radius * 1.2
    in
    [ List.range 0 (attributeAmount - 1)
        |> List.map
            (\r ->
                let
                    p =
                        Point2d.unsafe { x = toFloat size / 2 + radius, y = toFloat size / 2 }
                in
                (p
                    |> Point2d.rotateAround (Point2d.unsafe { x = toFloat size / 2, y = toFloat size / 2 })
                        (Angle.radians <| (2 * pi * toFloat r) / n)
                )
                    |> LineSegment2d.from
                        (p
                            |> Point2d.rotateAround (Point2d.unsafe { x = toFloat size / 2, y = toFloat size / 2 })
                                (Angle.radians <| (2 * pi * toFloat (r + attributeAmount)) / n)
                        )
                    |> Svg.lineSegment2d
                        [ Attributes.stroke <| "darkGray"
                        , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                        , Attributes.fill "none"
                        ]
            )
    , List.range 0 (attributeAmount - 1)
        |> List.map
            (\i ->
                let
                    p =
                        Point2d.unsafe
                            { x = toFloat size / 2 + textRadius
                            , y = toFloat size / 2
                            }
                            |> Point2d.rotateAround (Point2d.unsafe { x = toFloat size / 2, y = toFloat size / 2 })
                                (Angle.radians <| (2 * pi * toFloat i) / n)
                            |> Point2d.unwrap
                in
                Attribute.fromDegree (i + 1) False
                    |> Attribute.toString isGerman
                    |> Svg.text
                    |> List.singleton
                    |> Svg.text_
                        [ Attributes.x <| String.fromFloat <| p.x
                        , Attributes.y <| String.fromFloat <| p.y
                        , Attributes.textAnchor "middle"
                        ]
            )
    , List.range 0 (attributeAmount - 1)
        |> List.map
            (\i ->
                let
                    p =
                        Point2d.unsafe
                            { x = toFloat size / 2 + textRadius
                            , y = toFloat size / 2
                            }
                            |> Point2d.rotateAround (Point2d.unsafe { x = toFloat size / 2, y = toFloat size / 2 })
                                (Angle.radians <| (2 * pi * toFloat (i + attributeAmount)) / n)
                            |> Point2d.unwrap
                in
                Attribute.fromDegree (i + 1) True
                    |> Attribute.toString isGerman
                    |> Svg.text
                    |> List.singleton
                    |> Svg.text_
                        [ Attributes.x <| String.fromFloat <| p.x
                        , Attributes.y <| String.fromFloat <| p.y
                        , Attributes.textAnchor "middle"
                        ]
            )
    , [ List.range 0 n
            |> List.map
                (\r ->
                    Point2d.unsafe { x = toFloat size / 2 + radius, y = toFloat size / 2 }
                        |> Point2d.rotateAround (Point2d.unsafe { x = toFloat size / 2, y = toFloat size / 2 })
                            (Angle.radians <| (2 * pi * toFloat r) / n)
                )
            |> Polyline2d.fromVertices
            |> Svg.polyline2d
                [ Attributes.stroke <| "black"
                , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                , Attributes.fill "none"
                ]
      , List.range 0 n
            |> List.map
                (\r ->
                    Point2d.unsafe { x = toFloat size / 2 + (radius * 3 / 4), y = toFloat size / 2 }
                        |> Point2d.rotateAround (Point2d.unsafe { x = toFloat size / 2, y = toFloat size / 2 })
                            (Angle.radians <| (2 * pi * toFloat r) / n)
                )
            |> Polyline2d.fromVertices
            |> Svg.polyline2d
                [ Attributes.stroke <| "darkGray"
                , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                , Attributes.fill "none"
                ]
      , List.range 0 n
            |> List.map
                (\r ->
                    Point2d.unsafe { x = toFloat size / 2 + (radius * 2 / 4), y = toFloat size / 2 }
                        |> Point2d.rotateAround (Point2d.unsafe { x = toFloat size / 2, y = toFloat size / 2 })
                            (Angle.radians <| (2 * pi * toFloat r) / n)
                )
            |> Polyline2d.fromVertices
            |> Svg.polyline2d
                [ Attributes.stroke <| "darkGray"
                , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                , Attributes.fill "none"
                ]
      , List.range 0 n
            |> List.map
                (\r ->
                    Point2d.unsafe { x = toFloat size / 2 + (radius * 1 / 4), y = toFloat size / 2 }
                        |> Point2d.rotateAround (Point2d.unsafe { x = toFloat size / 2, y = toFloat size / 2 })
                            (Angle.radians <| (2 * pi * toFloat r) / n)
                )
            |> Polyline2d.fromVertices
            |> Svg.polyline2d
                [ Attributes.stroke <| "darkGray"
                , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                , Attributes.fill "none"
                ]
      , [ List.range 0 (attributeAmount - 1)
            |> List.map
                (\i ->
                    Point2d.unsafe
                        { x =
                            toFloat size
                                / 2
                                + (case array |> Array.get i of
                                    Just '0' ->
                                        diagramRadius

                                    Just '2' ->
                                        diagramRadius * 3 / 4

                                    _ ->
                                        diagramRadius / 2
                                  )
                        , y = toFloat size / 2
                        }
                        |> Point2d.rotateAround (Point2d.unsafe { x = toFloat size / 2, y = toFloat size / 2 })
                            (Angle.radians <| (2 * pi * toFloat i) / n)
                )
        , List.range 0 (attributeAmount - 1)
            |> List.map
                (\i ->
                    Point2d.unsafe
                        { x =
                            toFloat size
                                / 2
                                + (case array |> Array.get i of
                                    Just '1' ->
                                        diagramRadius

                                    Just '2' ->
                                        diagramRadius * 3 / 4

                                    _ ->
                                        diagramRadius / 2
                                  )
                        , y = toFloat size / 2
                        }
                        |> Point2d.rotateAround (Point2d.unsafe { x = toFloat size / 2, y = toFloat size / 2 })
                            (Angle.radians <| (2 * pi * toFloat (i + attributeAmount)) / n)
                )
        ]
            |> List.concat
            |> Polygon2d.singleLoop
            |> Svg.polygon2d
                [ Attributes.stroke <| "black"
                , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                , Attributes.fill <| "rgb(160,160,192)"
                , Attributes.fillOpacity <| "0.8"
                ]
      ]
    ]
        |> List.concat
