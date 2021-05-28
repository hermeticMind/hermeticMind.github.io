module View.MagicSquareSigil exposing (view)

import Angle
import Arc2d
import Array
import Circle2d
import Data.Alphabet as Alphabet
import Data.Turtle as Turtle exposing (Turtle)
import Direction2d exposing (Direction2d)
import Geometry.Svg as Svg
import Html exposing (Html)
import LineSegment2d
import List.Extra as List
import Point2d exposing (Point2d)
import Polyline2d
import Quantity exposing (Quantity(..))
import StaticArray exposing (StaticArray)
import StaticArray.Index exposing (Index)
import StaticArray.Length exposing (Length)
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import Vector2d


type alias N =
    Alphabet.TwentySix


n : Length N
n =
    Alphabet.twentySix


debugMode : Bool
debugMode =
    False


circlePoints : Int -> Float -> Point2d Float () -> List (Point2d Float ())
circlePoints max radius center =
    if max == 1 then
        center
            |> List.singleton

    else
        List.range 0 ((max - 1) * 4 - 1)
            |> List.map
                (\r ->
                    center
                        |> Point2d.translateBy (Vector2d.unsafe { x = radius, y = 0 })
                        |> Point2d.rotateAround center
                            (Angle.radians <| (2 * pi / toFloat ((max - 1) * 4)) * toFloat r)
                )


points : Int -> Float -> Point2d Float () -> StaticArray N (Point2d Float ())
points max radius center =
    let
        lookup =
            [ 2, 4, 6 ]
                |> List.map
                    (\max0 ->
                        circlePoints max0 (toFloat max0 * radius / toFloat max) center
                    )
                |> List.concat
                |> Array.fromList
    in
    StaticArray.fromList n
        0
        --1
        [ 1 --2
        , 2 --3
        , 3 --4
        , 4 --5
        , 5 --6
        , 6 --7
        , 7 --8
        , 8 --9
        , 9 --10
        , 10 --11
        , 11 --12
        , 12 --13
        , 13 --14
        , 14 --15
        , 15 --16
        , 16 --17
        , 17 --18
        , 18 --19
        , 19 --20
        , 20 --21
        , 21 --22
        , 22 --23
        , 23 --24
        , 24 --25
        , 25 --26
        ]
        |> StaticArray.map (\i -> lookup |> Array.get i |> Maybe.withDefault center)


turtle :
    { strokeWidth : Float, position : Point2d Float (), direction : Direction2d () }
    -> Turtle (List (Svg msg))
turtle { strokeWidth, position, direction } =
    let
        overshoot =
            0.05
    in
    { position = position
    , direction = direction
    , lineFun =
        \{ from, to } ->
            let
                dir =
                    Direction2d.from from to
                        |> Maybe.withDefault Direction2d.positiveX

                len =
                    Point2d.distanceFrom from to

                vec =
                    Vector2d.withLength (len |> Quantity.plus (Quantity.unsafe overshoot)) dir

                segment =
                    LineSegment2d.from from (from |> Point2d.translateBy vec)
            in
            [ segment
                |> Svg.lineSegment2d
                    [ Attributes.stroke "black"
                    , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                    ]
            ]
    , arcFun =
        \{ around, by, from } ->
            let
                arc =
                    Arc2d.sweptAround around
                        (by
                            |> Angle.inRadians
                            |> (\r ->
                                    if r < 0 then
                                        r - overshoot

                                    else
                                        r + overshoot
                               )
                            |> Angle.radians
                        )
                        from
            in
            [ arc
                |> Svg.arc2d
                    [ Attributes.fill <| "none"
                    , Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                    ]
            ]
    }


start : Float -> Turtle (List (Svg msg)) -> ( Turtle (List (Svg msg)), List (Svg msg) )
start radius =
    Turtle.rotateLeftBy (Angle.radians <| pi / 2)
        >> Turtle.jumpForward radius
        >> Turtle.rotateLeftBy (Angle.radians <| pi)
        >> Turtle.forwardBy (radius * 2)


end : Float -> ( Turtle (List (Svg msg)), List (Svg msg) ) -> List (Svg msg)
end radius =
    Turtle.map (Turtle.rotateRightBy (Angle.radians <| pi / 2))
        >> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| 0
                , radius = radius
                }
            )
        >> Turtle.map (Turtle.rotateLeftBy (Angle.radians <| pi / 2))
        >> Tuple.second


viewWord :
    { strokeWidth : Float
    }
    -> List (Point2d Float ())
    -> List (Svg msg)
viewWord { strokeWidth } path =
    let
        radius =
            strokeWidth * 2
    in
    [ case path of
        a :: b :: _ ->
            start radius
                (turtle
                    { strokeWidth = strokeWidth
                    , position = a
                    , direction = Direction2d.from a b |> Maybe.withDefault Direction2d.positiveX
                    }
                )
                |> Tuple.second

        _ ->
            []
    , path
        |> Polyline2d.fromVertices
        |> Svg.polyline2d
            [ Attributes.stroke <| "black"
            , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
            , Attributes.fill <| "none"
            ]
        |> List.singleton
    , case path |> List.reverse of
        a :: b :: _ ->
            turtle
                { strokeWidth = strokeWidth
                , position = a
                , direction = Direction2d.from b a |> Maybe.withDefault Direction2d.positiveX
                }
                |> Turtle.init []
                |> end radius

        _ ->
            []
    ]
        |> List.concat


view :
    { size : Float
    , radius : Float
    , zoom : Float
    , strokeWidth : Float
    , alphabet : Char -> Index Alphabet.TwentySix
    , withText : Bool
    , withBorder : Bool
    , withCircle : Bool
    }
    -> String
    -> Html msg
view config inputText =
    let
        p =
            points 6 (config.radius - config.strokeWidth * 2) (Point2d.unsafe { x = config.size / 2, y = config.size / 2 })
    in
    inputText
        |> String.words
        |> List.concatMap
            (String.toList
                >> List.map
                    (config.alphabet
                        >> (\i ->
                                p
                                    |> StaticArray.get i
                           )
                    )
                >> viewWord
                    { strokeWidth = config.strokeWidth
                    }
            )
        |> List.append
            (if config.withText then
                [ Svg.text_
                    [ Attributes.fontFamily "Dancing Script, serif"
                    , config.size / 2 |> String.fromFloat |> Attributes.x
                    , config.size
                        - config.strokeWidth
                        * 8
                        |> String.fromFloat
                        |> Attributes.y
                    , Attributes.textAnchor "middle"
                    , Attributes.alignmentBaseline "central"
                    ]
                    [ Svg.text inputText ]
                ]

             else
                []
            )
        |> List.append
            (if debugMode then
                p
                    |> StaticArray.toList
                    |> List.map
                        (Circle2d.withRadius (Quantity 1)
                            >> Svg.circle2d
                                [ Attributes.fill "black"
                                ]
                        )

             else
                []
            )
        |> (if config.withCircle then
                List.append
                    (Circle2d.atPoint (Point2d.unsafe { x = config.size / 2, y = config.size / 2 })
                        (Quantity <| config.radius)
                        |> Svg.circle2d
                            [ Attributes.fill <| "none"
                            , Attributes.stroke <| "black"
                            , Attributes.strokeWidth <| String.fromFloat <| config.strokeWidth
                            ]
                        |> List.singleton
                    )

            else
                identity
           )
        |> List.append
            (if config.withBorder then
                { position = Point2d.unsafe { x = config.size / 2, y = config.strokeWidth * 3 }
                , direction = Direction2d.positiveX
                , lineFun =
                    \{ from, to } ->
                        let
                            segment =
                                LineSegment2d.from from to
                        in
                        [ segment
                            |> Svg.lineSegment2d
                                [ Attributes.stroke "black"
                                , Attributes.strokeWidth <| String.fromFloat <| config.strokeWidth / 2
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
                                , Attributes.stroke <| "black"
                                , Attributes.strokeWidth <| String.fromFloat <| config.strokeWidth / 2
                                ]
                        ]
                }
                    |> Turtle.forwardBy (config.size / 2 - config.strokeWidth * 3 + config.strokeWidth / 2)
                    |> Turtle.andThen (Turtle.arcLeftTo { direction = Direction2d.positiveY, radius = config.strokeWidth })
                    |> Turtle.andThen (Turtle.forwardBy (config.size - config.strokeWidth * 6 + config.strokeWidth))
                    |> Turtle.andThen (Turtle.arcLeftTo { direction = Direction2d.negativeX, radius = config.strokeWidth })
                    |> Turtle.andThen (Turtle.forwardBy (config.size - config.strokeWidth * 6 + config.strokeWidth))
                    |> Turtle.andThen (Turtle.arcLeftTo { direction = Direction2d.negativeY, radius = config.strokeWidth })
                    |> Turtle.andThen (Turtle.forwardBy (config.size - config.strokeWidth * 6 + config.strokeWidth))
                    |> Turtle.andThen (Turtle.arcLeftTo { direction = Direction2d.positiveX, radius = config.strokeWidth })
                    |> Turtle.andThen (Turtle.forwardBy (config.size / 2 - config.strokeWidth * 3 + config.strokeWidth / 2))
                    |> Tuple.second

             else
                []
            )
        |> Svg.svg
            [ Attributes.width <| (String.fromFloat <| config.zoom * config.size) ++ "px"
            , Attributes.height <| (String.fromFloat <| config.zoom * config.size) ++ "px"
            , Attributes.version <| "1.1"
            , Attributes.viewBox <|
                "0 0 "
                    ++ String.fromFloat config.size
                    ++ " "
                    ++ String.fromFloat config.size
            ]
