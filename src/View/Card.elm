module View.Card exposing (defaultConfig, padding, view, viewSymbol)

import Angle
import Circle2d
import Data.Card as Card exposing (Card(..))
import Direction2d
import Geometry.Svg as Svg
import Pixels
import Point2d
import Polygon2d
import Polyline2d
import Rectangle2d
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import View.BinarySigil as BinarySigil
import View.Color as Color
import View.RegularPolygon as RegularPolygon



--------------------------------------------------------------------------------
-- Config
--------------------------------------------------------------------------------


type alias Config =
    { width : Float
    , height : Float
    , relative : Float -> Float
    , radius : Float
    , bigRadius : Float
    }


defaultConfig : Config
defaultConfig =
    let
        width : Float
        width =
            --52
            -- 70
            898

        height : Float
        height =
            --86
            --120
            1488

        radius : Float
        radius =
            --relative 4
            (width - padding) / 7

        bigRadius : Float
        bigRadius =
            -- radius * 5 / 4
            2 * radius / sqrt 2

        relative : Float -> Float
        relative =
            let
                factor =
                    if height > width then
                        height / width

                    else
                        width / height
            in
            --factor
            (*) (factor * width / 70)
    in
    { width = width
    , height = height
    , relative = relative
    , radius = radius
    , bigRadius = bigRadius
    }


padding : Float
padding =
    --relative 7
    12 * (3 + (4 * 2))



--------------------------------------------------------------------------------
-- Views
--------------------------------------------------------------------------------


viewElement : Config -> Int -> List (Svg msg)
viewElement { width, height, relative, radius, bigRadius } n =
    case n of
        1 ->
            --Earth
            [ RegularPolygon.view
                { n = 3, scale = radius, standing = True }
                ( width / 2, height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| Color.green
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    , Attributes.fill "none"
                    ]
            , Circle2d.atPoint
                (Point2d.pixels (width / 2) (height / 2))
                (Pixels.pixels <| relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| Color.green
                    ]
            , Circle2d.atPoint
                (Point2d.pixels (width / 2) (height / 2 + bigRadius / 4))
                (Pixels.pixels <| relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| Color.black
                    ]
            , Circle2d.atPoint
                (Point2d.pixels (width / 2) (height / 2 - bigRadius / 4))
                (Pixels.pixels <| relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| Color.black
                    ]
            ]

        2 ->
            --Fire
            [ RegularPolygon.view
                { n = 3, scale = radius, standing = True }
                ( width / 2, height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| Color.red
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    , Attributes.fill "none"
                    ]
            , Circle2d.atPoint
                (Point2d.pixels (width / 2) (height / 2))
                (Pixels.pixels <| relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| Color.red
                    ]
            ]

        3 ->
            --Air
            [ RegularPolygon.view
                { n = 3, scale = radius, standing = True }
                ( width / 2, height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| Color.yellow
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    , Attributes.fill "none"
                    ]
            , Polyline2d.fromVertices
                [ Point2d.pixels (width / 2) (height / 2 + (bigRadius / 2) - (relative <| 1))
                , Point2d.pixels (width / 2) (height / 2 - (bigRadius / 2) - (relative <| 1))
                ]
                |> Svg.polyline2d
                    [ Attributes.stroke <| Color.black
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    , Attributes.fill "none"
                    ]
            ]

        4 ->
            --Water
            [ RegularPolygon.view
                { n = 4, scale = radius, standing = True }
                ( width / 2, height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| Color.blue
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    , Attributes.fill "none"
                    ]
            , Polyline2d.fromVertices
                [ Point2d.pixels 0 (height / 2)
                , Point2d.pixels width (height / 2)
                ]
                |> Svg.polyline2d
                    [ Attributes.stroke <| Color.black
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    , Attributes.fill "none"
                    ]
            , Polyline2d.fromVertices
                [ Point2d.pixels (width / 2) (height / 2 + (bigRadius / 2))
                , Point2d.pixels (width / 2) (height / 2 - (bigRadius / 2))
                ]
                |> Svg.polyline2d
                    [ Attributes.stroke <| Color.black
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    , Attributes.fill "none"
                    ]
            ]

        _ ->
            []


viewPlanet : Config -> Int -> List (Svg msg)
viewPlanet { width, height, relative, radius, bigRadius } n =
    case n of
        1 ->
            --Wissen
            [ RegularPolygon.view
                { n = 4, scale = radius / 2, standing = True }
                ( width / 2, height / 2 - bigRadius - radius / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| Color.black
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    , Attributes.fill "none"
                    ]
            , Circle2d.atPoint (Point2d.pixels (width / 2) (height / 2))
                (Pixels.pixels <| bigRadius / 2)
                |> Svg.circle2d
                    [ Attributes.stroke <| Color.black
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    , Attributes.fill <| "none"
                    ]
            , Polyline2d.fromVertices
                [ Point2d.pixels (width / 2) (height / 2)
                , Point2d.pixels (width / 2) (height / 2 - bigRadius - relative 0.5)
                ]
                |> Svg.polyline2d
                    [ Attributes.stroke <| Color.black
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    , Attributes.fill "none"
                    ]
            ]

        2 ->
            --Venus
            [ RegularPolygon.view
                { n = 3, scale = radius * 2 + relative 1, standing = True }
                ( width / 2, height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| Color.black
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    , Attributes.fill "none"
                    ]
            , RegularPolygon.view
                { n = 3, scale = radius, standing = False }
                ( width / 2, height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| Color.black
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    , Attributes.fill "none"
                    ]
            ]

        3 ->
            --Erde
            [ RegularPolygon.view
                { n = 3, scale = radius, standing = True }
                ( width / 2, height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| Color.black
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    , Attributes.fill "none"
                    ]
            , Circle2d.atPoint
                (Point2d.pixels (width / 2) (height / 2))
                (Pixels.pixels <| relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| Color.black
                    ]
            ]
                ++ (List.range 1 4
                        |> List.map
                            (\r ->
                                Circle2d.atPoint
                                    (Point2d.pixels (width / 2) (height / 2 - bigRadius * 2)
                                        |> Point2d.rotateAround (Point2d.pixels (width / 2) (height / 2)) (Angle.radians <| pi * toFloat r / 2)
                                    )
                                    (Pixels.pixels <| relative <| 1 / 2)
                                    |> Svg.circle2d
                                        [ Attributes.fill <| Color.black
                                        ]
                            )
                   )

        4 ->
            --Mars
            [ RegularPolygon.view
                { n = 4, scale = radius, standing = True }
                ( width / 2, height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| Color.black
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    , Attributes.fill "none"
                    ]
            , RegularPolygon.view
                { n = 4, scale = radius * 2, standing = True }
                ( width / 2, height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| Color.black
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    , Attributes.fill "none"
                    ]
            ]

        5 ->
            --Jupiter
            [ RegularPolygon.view
                { n = 4, scale = radius, standing = True }
                ( width / 2, height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| Color.black
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    , Attributes.fill "none"
                    ]
            , Circle2d.atPoint
                (Point2d.pixels (width / 2) (height / 2))
                (Pixels.pixels <| relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| Color.black
                    ]
            ]
                ++ (List.range 1 8
                        |> List.map
                            (\r ->
                                Circle2d.atPoint
                                    (Point2d.pixels (width / 2) (height / 2 - bigRadius * 2)
                                        |> Point2d.rotateAround (Point2d.pixels (width / 2) (height / 2))
                                            (Angle.radians <| pi * 2 * toFloat (1 + r) / 8)
                                    )
                                    (Pixels.pixels <| relative <| 1 / 2)
                                    |> Svg.circle2d
                                        [ Attributes.fill <| Color.black
                                        ]
                            )
                   )

        6 ->
            --Saturn
            [ RegularPolygon.view
                { n = 4, scale = radius, standing = True }
                ( width / 2, height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| Color.black
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    , Attributes.fill "none"
                    ]
            , Circle2d.atPoint
                (Point2d.pixels (width / 2) (height / 2))
                (Pixels.pixels <| relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| Color.black
                    ]
            , Circle2d.atPoint
                (Point2d.pixels (width / 2 + bigRadius) (height / 2))
                (Pixels.pixels <| relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| Color.black
                    ]
            , Circle2d.atPoint
                (Point2d.pixels (width / 2 + bigRadius * 2) (height / 2))
                (Pixels.pixels <| relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| Color.black
                    ]
            ]

        7 ->
            -- Uranus
            [ RegularPolygon.view
                { n = 4, scale = radius, standing = True }
                ( width / 2, height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| Color.black
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    , Attributes.fill "none"
                    ]
            , Circle2d.atPoint
                (Point2d.pixels (width / 2) (height / 2))
                (Pixels.pixels <| relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| Color.black
                    ]
            ]
                ++ (List.range 1 4
                        |> List.map
                            (\r ->
                                Circle2d.atPoint
                                    (Point2d.pixels (width / 2) (height / 2 - bigRadius * 2)
                                        |> Point2d.rotateAround (Point2d.pixels (width / 2) (height / 2))
                                            (Angle.radians <| pi * 2 * toFloat (1 + r) / 4)
                                    )
                                    (Pixels.pixels <| relative <| 1 / 2)
                                    |> Svg.circle2d
                                        [ Attributes.fill <| Color.black
                                        ]
                            )
                   )

        8 ->
            --Neptun
            [ RegularPolygon.view
                { n = 4, scale = radius, standing = True }
                ( width / 2, height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| Color.black
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    , Attributes.fill "none"
                    ]
            , Polyline2d.fromVertices
                [ Point2d.pixels (width / 2) (height / 2)
                , Point2d.pixels (width / 2) (height / 2 + bigRadius)
                ]
                |> Svg.polyline2d
                    [ Attributes.stroke <| Color.black
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    , Attributes.fill "none"
                    ]
            ]

        _ ->
            []


viewTrump : Config -> Int -> List (Svg msg)
viewTrump { width, height, relative, radius, bigRadius } n =
    case n of
        1 ->
            [ Circle2d.atPoint (Point2d.pixels (width / 2) (height / 2))
                (Pixels.pixels <| bigRadius / 2)
                |> Svg.circle2d
                    [ Attributes.stroke <| Color.black
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    , Attributes.fill <| "none"
                    ]
            , Polygon2d.singleLoop
                [ Point2d.pixels (width / 2) (height / 2 - bigRadius)
                , Point2d.pixels (width / 2) (height / 2)
                ]
                |> Svg.polygon2d
                    [ Attributes.stroke <| Card.color <| Trump n
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    ]
            ]

        2 ->
            [ RegularPolygon.view
                { n = 3, scale = radius, standing = True }
                ( width / 2, height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| Color.black
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    , Attributes.fill "none"
                    ]
            , Circle2d.atPoint (Point2d.pixels (width / 2) (height / 2))
                (Pixels.pixels <| relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| Color.black
                    ]
            , Circle2d.atPoint (Point2d.pixels (width / 2) (height / 2 - bigRadius))
                (Pixels.pixels <| relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| Card.color <| Trump n
                    ]
            , Circle2d.atPoint (Point2d.pixels (width / 2) (height / 2 - bigRadius * 2))
                (Pixels.pixels <| relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| Card.color <| Trump n
                    ]
            ]

        3 ->
            []

        4 ->
            []

        5 ->
            [ Circle2d.atPoint (Point2d.pixels (width / 2) (height / 2))
                (Pixels.pixels <| bigRadius / 2)
                |> Svg.circle2d
                    [ Attributes.stroke <| Color.black
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    , Attributes.fill <| "none"
                    ]
            , Circle2d.atPoint
                (Point2d.pixels (width / 2) (height / 2))
                (Pixels.pixels <| relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| Color.black
                    ]
            ]

        6 ->
            []

        7 ->
            []

        8 ->
            [ RegularPolygon.view
                { n = 4, scale = radius, standing = True }
                ( width / 2, height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| Color.black
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    , Attributes.fill "none"
                    ]
            , RegularPolygon.view
                { n = 4, scale = radius / 2, standing = True }
                ( width / 2, height / 2 - bigRadius - radius / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| Card.color <| Trump n
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    , Attributes.fill "none"
                    ]
            , Polygon2d.singleLoop
                [ Point2d.pixels (width / 2) (height / 2 - bigRadius - relative 1 / 2)
                , Point2d.pixels (width / 2) (height / 2)
                ]
                |> Svg.polygon2d
                    [ Attributes.stroke <| Card.color <| Trump n
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    ]
            ]

        9 ->
            [ Circle2d.atPoint (Point2d.pixels (width / 2) (height / 2))
                (Pixels.pixels <| bigRadius / 2)
                |> Svg.circle2d
                    [ Attributes.stroke <| Color.black
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    , Attributes.fill "none"
                    ]
            , Circle2d.atPoint (Point2d.pixels (width / 2) (height / 2))
                (Pixels.pixels <| relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| Color.black
                    ]
            , Circle2d.atPoint (Point2d.pixels (width / 2 + bigRadius) (height / 2))
                (Pixels.pixels <| relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| Card.color <| Trump n
                    ]
            , Circle2d.atPoint (Point2d.pixels (width / 2 + bigRadius * 2) (height / 2))
                (Pixels.pixels <| relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| Card.color <| Trump n
                    ]
            ]

        10 ->
            [ RegularPolygon.view
                { n = 3, scale = radius, standing = True }
                ( width / 2, height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| Color.black
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    , Attributes.fill "none"
                    ]
            , Circle2d.atPoint (Point2d.pixels (width / 2 + bigRadius + radius / 2) (height / 2))
                (Pixels.pixels <| radius * 1 / 2)
                |> Svg.circle2d
                    [ Attributes.stroke <| Card.color <| Trump n
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    , Attributes.fill <| "none"
                    ]
            , Polygon2d.singleLoop
                [ Point2d.pixels (width / 2 + bigRadius) (height / 2)
                , Point2d.pixels (width / 2) (height / 2)
                ]
                |> Svg.polygon2d
                    [ Attributes.stroke <| Card.color <| Trump n
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    ]
            ]

        11 ->
            []

        12 ->
            [ RegularPolygon.view
                { n = 4, scale = radius * 1 / 2, standing = True }
                ( width / 2, height / 2 - radius / (2 * sqrt 2) - relative 1 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| Card.color <| Trump n
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    , Attributes.fill "none"
                    ]
            , Polygon2d.singleLoop
                [ Point2d.pixels 0 (height / 2)
                , Point2d.pixels width (height / 2)
                ]
                |> Svg.polygon2d
                    [ Attributes.stroke <| Color.black
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    ]
            ]

        13 ->
            []

        14 ->
            []

        15 ->
            [ RegularPolygon.view
                { n = 3, scale = radius, standing = True }
                ( width / 2, height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| Color.black
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    , Attributes.fill "none"
                    ]
            , RegularPolygon.view
                { n = 4, scale = radius / 2, standing = True }
                ( width / 2, height / 2 - bigRadius - radius / (2 * sqrt 2) )
                |> Svg.polygon2d
                    [ Attributes.stroke <| Card.color <| Trump n
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    , Attributes.fill "none"
                    ]
            , Polygon2d.singleLoop
                [ Point2d.pixels (width / 2) (height / 2 - bigRadius)
                , Point2d.pixels (width / 2) (height / 2)
                ]
                |> Svg.polygon2d
                    [ Attributes.stroke <| Card.color <| Trump n
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    ]
            ]

        16 ->
            []

        17 ->
            []

        18 ->
            [ RegularPolygon.view
                { n = 3, scale = radius, standing = True }
                ( width / 2, height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| Color.black
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    , Attributes.fill "none"
                    ]
            , Polygon2d.singleLoop
                [ Point2d.pixels (width / 2) (height / 2 - bigRadius)
                , Point2d.pixels (width / 2) (height / 2)
                ]
                |> Svg.polygon2d
                    [ Attributes.stroke <| Card.color <| Trump n
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    ]
            ]

        19 ->
            []

        20 ->
            []

        21 ->
            []

        _ ->
            []


viewBack : Config -> List (Svg msg)
viewBack { width, height, relative, radius, bigRadius } =
    [ Circle2d.atPoint (Point2d.pixels (width / 2) (height / 2))
        (Pixels.pixels <| radius)
        |> Svg.circle2d
            [ Attributes.stroke <| "white"
            , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
            , Attributes.fill <| "none"
            ]
    ]


viewVirtue : Config -> Int -> List (Svg msg)
viewVirtue { width, height, relative, radius, bigRadius } n =
    case n of
        1 ->
            --Mitgef??hl
            [ RegularPolygon.view
                { n = 3, scale = radius, standing = True }
                ( width / 2 - radius + relative 1 / 2, height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| Color.black
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    , Attributes.fill "none"
                    ]
            , RegularPolygon.view
                { n = 3, scale = radius, standing = True }
                ( width / 2 + radius - relative 1 / 2, height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| Color.black
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    , Attributes.fill "none"
                    ]
            , Polygon2d.singleLoop
                [ Point2d.pixels (width / 4) (height / 2 - radius - relative 1)
                , Point2d.pixels (width * 3 / 4) (height / 2 - radius - relative 1)
                ]
                |> Svg.polygon2d
                    [ Attributes.stroke <| Card.color <| Virtue n
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    ]
            ]

        2 ->
            --Freundlichkeit
            [ RegularPolygon.view
                { n = 3, scale = radius, standing = True }
                ( width / 2, height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| Color.black
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    , Attributes.fill "none"
                    ]
            , Circle2d.atPoint (Point2d.pixels (width / 2) (height / 2))
                (Pixels.pixels <| relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| Color.black
                    ]
            ]
                ++ (List.range 1 5
                        |> List.map
                            (\r ->
                                Circle2d.atPoint
                                    (Point2d.pixels (width / 2) (height / 2 - bigRadius * 2)
                                        |> Point2d.rotateAround (Point2d.pixels (width / 2) (height / 2)) (Angle.radians <| pi * toFloat (5 + r) / 4)
                                    )
                                    (Pixels.pixels <| relative <| 1 / 2)
                                    |> Svg.circle2d
                                        [ Attributes.fill <|
                                            if r + 1 |> modBy 2 |> (==) 0 then
                                                Card.color <| Virtue n

                                            else
                                                Color.black
                                        ]
                            )
                   )

        3 ->
            --Offenheit
            [ Circle2d.atPoint (Point2d.pixels (width / 2) (height / 2))
                (Pixels.pixels <| bigRadius / 2)
                |> Svg.circle2d
                    [ Attributes.stroke <| Color.black
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    , Attributes.fill <| "none"
                    ]
            , Circle2d.atPoint (Point2d.pixels (width / 2) (height / 2))
                (Pixels.pixels <| relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| Color.black
                    ]
            , Circle2d.atPoint (Point2d.pixels (width / 2) (height / 2 - bigRadius))
                (Pixels.pixels <| relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| Card.color <| Virtue n
                    ]
            , Circle2d.atPoint (Point2d.pixels (width / 2) (height / 2 - bigRadius * 2))
                (Pixels.pixels <| relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| Card.color <| Virtue n
                    ]
            ]

        4 ->
            --Vergebung
            [ RegularPolygon.view
                { n = 3, scale = radius, standing = True }
                ( width / 2, height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| Color.black
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    , Attributes.fill "white"
                    ]
            , Polygon2d.singleLoop
                [ Point2d.pixels 0 (height / 2)
                , Point2d.pixels (width / 4) (height / 2)
                ]
                |> Svg.polygon2d
                    [ Attributes.stroke <| Color.black
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    ]
            , Polygon2d.singleLoop
                [ Point2d.pixels (width / 4) ((height / 2) - relative 2)
                , Point2d.pixels (width / 4) ((height / 2) + relative 2)
                ]
                |> Svg.polygon2d
                    [ Attributes.stroke <| Color.black
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    ]
            ]

        5 ->
            --Geduld
            [ RegularPolygon.view
                { n = 3, scale = radius, standing = True }
                ( width / 2, height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| Color.black
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    , Attributes.fill "none"
                    ]
            , RegularPolygon.view
                { n = 4, scale = radius / 2, standing = True }
                ( width / 2 + bigRadius + radius / 2, height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| Card.color <| Virtue n
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    , Attributes.fill <| "none"
                    ]
            , Polygon2d.singleLoop
                [ Point2d.pixels (width / 2 + bigRadius + relative (1 / 2)) (height / 2)
                , Point2d.pixels (width / 2) (height / 2)
                ]
                |> Svg.polygon2d
                    [ Attributes.stroke <| Card.color <| Virtue n
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    ]
            ]

        6 ->
            --True
            [ RegularPolygon.view
                { n = 3, scale = radius, standing = True }
                ( width / 2, height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| Color.black
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    , Attributes.fill "none"
                    ]
            , RegularPolygon.view
                { n = 3, scale = radius / 2, standing = True }
                ( width / 2, height / 2 - bigRadius - radius / (2 * sqrt 2) )
                |> Svg.polygon2d
                    [ Attributes.stroke <| Color.black
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    , Attributes.fill "none"
                    ]
            , Polygon2d.singleLoop
                [ Point2d.pixels (width / 2) (height / 2 - bigRadius - (relative <| 0.5))
                , Point2d.pixels (width / 2) (height / 2)
                ]
                |> Svg.polygon2d
                    [ Attributes.stroke <| Color.black
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    ]
            ]

        7 ->
            --Selbstbeherrschung
            [ RegularPolygon.view
                { n = 3, scale = radius, standing = True }
                ( width / 2, height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| Color.black
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    , Attributes.fill "none"
                    ]
            , Circle2d.atPoint (Point2d.pixels (width / 2) (height / 2))
                (Pixels.pixels <| relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| Color.black
                    ]
            ]
                ++ (List.range 1 8
                        |> List.map
                            (\r ->
                                Circle2d.atPoint
                                    (Point2d.pixels (width / 2) (height / 2 - bigRadius * 2)
                                        |> Point2d.rotateAround (Point2d.pixels (width / 2) (height / 2)) (Angle.radians <| pi * toFloat r / 4)
                                    )
                                    (Pixels.pixels <| relative <| 1 / 2)
                                    |> Svg.circle2d
                                        [ Attributes.fill <|
                                            if r |> modBy 2 |> (==) 0 then
                                                Card.color <| Virtue n

                                            else
                                                Color.black
                                        ]
                            )
                   )

        8 ->
            --Ausdauer
            [ RegularPolygon.view
                { n = 3, scale = radius, standing = True }
                ( width / 2, height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| Color.black
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    , Attributes.fill "none"
                    ]
            , Polygon2d.singleLoop
                [ Point2d.pixels 0 (height / 2)
                , Point2d.pixels width (height / 2)
                ]
                |> Svg.polygon2d
                    [ Attributes.stroke <| Color.black
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    ]
            ]

        9 ->
            --Selbsterkenntnis
            [ RegularPolygon.view
                { n = 3, scale = radius, standing = True }
                ( width / 2, height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| Color.black
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    , Attributes.fill "none"
                    ]
            , Circle2d.atPoint (Point2d.pixels (width / 2) (height / 2))
                (Pixels.pixels <| relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| Color.black
                    ]
            , Circle2d.atPoint (Point2d.pixels (width / 2) (height / 2 + bigRadius))
                (Pixels.pixels <| relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| Card.color <| Virtue n
                    ]
            , Circle2d.atPoint (Point2d.pixels (width / 2) (height / 2 + bigRadius * 2))
                (Pixels.pixels <| relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| Card.color <| Virtue n
                    ]
            ]

        10 ->
            --Autentizit??t
            [ RegularPolygon.view
                { n = 3, scale = radius, standing = True }
                ( width / 2, height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| Color.black
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    , Attributes.fill "none"
                    ]
            , Circle2d.atPoint
                (Point2d.pixels (width / 2) (height / 2))
                (Pixels.pixels <| relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| Color.black
                    ]
            ]
                ++ (List.range 1 3
                        |> List.map
                            (\r ->
                                Circle2d.atPoint
                                    (Point2d.pixels (width / 2) (height / 2 - bigRadius * 2)
                                        |> Point2d.rotateAround (Point2d.pixels (width / 2) (height / 2))
                                            (Angle.radians <| pi * 2 * toFloat (1 + r) / 3)
                                    )
                                    (Pixels.pixels <| relative <| 1 / 2)
                                    |> Svg.circle2d
                                        [ Attributes.fill <|
                                            if r |> modBy 2 |> (==) 0 then
                                                Card.color <| Virtue n

                                            else
                                                Color.black
                                        ]
                            )
                   )

        11 ->
            --Ehrlichkeit
            [ RegularPolygon.view
                { n = 3, scale = radius, standing = True }
                ( width / 2, height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| Color.black
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    , Attributes.fill "none"
                    ]
            , RegularPolygon.view
                { n = 4, scale = radius * 2, standing = True }
                ( width / 2, height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| Card.color <| Virtue n
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    , Attributes.fill "none"
                    ]
            , Polygon2d.singleLoop
                [ Point2d.pixels (width / 2 + bigRadius) (height / 2)
                , Point2d.pixels (width / 2) (height / 2)
                ]
                |> Svg.polygon2d
                    [ Attributes.stroke <| Card.color <| Virtue n
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    ]
            ]

        12 ->
            --M????igkeit
            [ RegularPolygon.view
                { n = 4, scale = radius, standing = True }
                ( width / 2, height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| Color.black
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    , Attributes.fill "none"
                    ]
            , Circle2d.atPoint
                (Point2d.pixels (width / 2) (height / 2))
                (Pixels.pixels <| relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| Color.black
                    ]
            ]
                ++ (List.range 1 4
                        |> List.map
                            (\r ->
                                Circle2d.atPoint
                                    (Point2d.pixels (width / 2) (height / 2 - bigRadius / 4)
                                        |> Point2d.rotateAround (Point2d.pixels (width / 2) (height / 2)) (Angle.radians <| pi * toFloat r / 2)
                                    )
                                    (Pixels.pixels <| relative <| 1 / 2)
                                    |> Svg.circle2d
                                        [ Attributes.fill <| Card.color <| Virtue n
                                        ]
                            )
                   )

        13 ->
            --Humor
            [ Circle2d.atPoint (Point2d.pixels (width / 2) (height / 2))
                (Pixels.pixels <| bigRadius / 2)
                |> Svg.circle2d
                    [ Attributes.stroke <| Color.black
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    , Attributes.fill <| "none"
                    ]
            , Circle2d.atPoint (Point2d.pixels (width / 2) (height / 2))
                (Pixels.pixels <| relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| Color.black
                    ]
            ]
                ++ (List.range 1 5
                        |> List.map
                            (\r ->
                                Circle2d.atPoint
                                    (Point2d.pixels (width / 2) (height / 2 - bigRadius * 2)
                                        |> Point2d.rotateAround (Point2d.pixels (width / 2) (height / 2)) (Angle.radians <| pi * toFloat (5 + r) / 4)
                                    )
                                    (Pixels.pixels <| relative <| 1 / 2)
                                    |> Svg.circle2d
                                        [ Attributes.fill <|
                                            if r + 1 |> modBy 2 |> (==) 0 then
                                                Card.color <| Virtue n

                                            else
                                                Color.black
                                        ]
                            )
                   )

        14 ->
            --Hoffnung
            [ RegularPolygon.view
                { n = 3, scale = radius, standing = True }
                ( width / 2, height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| Color.black
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    , Attributes.fill "none"
                    ]
            , Circle2d.atPoint (Point2d.pixels (width / 2) (height / 2))
                (Pixels.pixels <| relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| Color.black
                    ]
            , Circle2d.atPoint (Point2d.pixels (width / 2 + bigRadius) (height / 2))
                (Pixels.pixels <| relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| Card.color <| Virtue n
                    ]
            , Circle2d.atPoint (Point2d.pixels (width / 2 + bigRadius * 2) (height / 2))
                (Pixels.pixels <| relative <| 1 / 2)
                |> Svg.circle2d
                    [ Attributes.fill <| Card.color <| Virtue n
                    ]
            ]

        15 ->
            --Mut
            [ RegularPolygon.view
                { n = 4, scale = radius / 2, standing = True }
                ( width / 2, height / 2 - (height - padding) / 4 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| Card.color <| Virtue n
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    , Attributes.fill "none"
                    ]
            , Polygon2d.singleLoop
                [ Point2d.pixels 0 (height / 2)
                , Point2d.pixels width (height / 2)
                ]
                |> Svg.polygon2d
                    [ Attributes.stroke <| Color.black
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    ]
            ]

        16 ->
            [ RegularPolygon.view
                { n = 4, scale = radius, standing = True }
                ( width / 2, height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| Color.black
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    , Attributes.fill "none"
                    ]
            , RegularPolygon.view
                { n = 4, scale = radius / 2, standing = True }
                ( width / 2 + bigRadius + radius / 2, height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| Card.color <| Virtue n
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    , Attributes.fill <| "none"
                    ]
            , Polygon2d.singleLoop
                [ Point2d.pixels (width / 2 + bigRadius + relative (1 / 2)) (height / 2)
                , Point2d.pixels (width / 2 - bigRadius) (height / 2)
                ]
                |> Svg.polygon2d
                    [ Attributes.stroke <| Card.color <| Virtue n
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    ]
            ]

        _ ->
            []


viewSymbol : Config -> Card -> List (Svg msg)
viewSymbol ({ width, height, relative, radius, bigRadius } as config) card =
    case card of
        Trump n ->
            viewTrump config n

        Element n ->
            viewElement config n

        Planet n ->
            viewPlanet config n

        Joker ->
            [ RegularPolygon.view
                { n = 3, scale = radius * 2, standing = True }
                ( width / 2, height / 2 )
                |> Svg.polygon2d
                    [ Attributes.stroke <| Color.black
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    , Attributes.fill "none"
                    ]
            , Circle2d.atPoint (Point2d.pixels (width / 2) (height / 2))
                (Pixels.pixels <| bigRadius / 2)
                |> Svg.circle2d
                    [ Attributes.stroke <| Color.black
                    , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                    , Attributes.fill "none"
                    ]
            ]

        Binary n ->
            case n of
                2 ->
                    [ RegularPolygon.view
                        { n = 3, scale = radius, standing = True }
                        ( width / 2, height / 2 )
                        |> Svg.polygon2d
                            [ Attributes.stroke <| Color.black
                            , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                            , Attributes.fill "none"
                            ]
                    , RegularPolygon.view
                        { n = 4, scale = radius * 2, standing = True }
                        ( width / 2, height / 2 )
                        |> Svg.polygon2d
                            [ Attributes.stroke <| Color.black
                            , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                            , Attributes.fill "none"
                            ]
                    ]

                1 ->
                    [ RegularPolygon.view
                        { n = 3, scale = radius, standing = True }
                        ( width / 2, height / 2 )
                        |> Svg.polygon2d
                            [ Attributes.stroke <| "white"
                            , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                            , Attributes.fill "none"
                            ]
                    , RegularPolygon.view
                        { n = 3, scale = radius * 1 / 2, standing = True }
                        ( width / 2, height / 2 - bigRadius - radius / 2 )
                        |> Polygon2d.rotateAround (Point2d.pixels (width / 2) (height / 2)) (Angle.radians <| pi / 2)
                        |> Svg.polygon2d
                            [ Attributes.stroke <| "white"
                            , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                            , Attributes.fill "none"
                            ]
                    , Polygon2d.singleLoop
                        [ Point2d.pixels (width / 2) (height / 2)
                        , Point2d.pixels (width / 2 + bigRadius + relative 1) (height / 2)
                        ]
                        |> Svg.polygon2d
                            [ Attributes.stroke <| "white"
                            , Attributes.strokeWidth <| String.fromFloat <| relative <| 1
                            ]
                    ]

                _ ->
                    []

        Virtue n ->
            viewVirtue config n

        Back ->
            viewBack config


viewSigil : Config -> Card -> List (Svg msg)
viewSigil { width, height, relative, radius, bigRadius } card =
    case card of
        Joker ->
            { value = Card.value card - 1
            , size = 0
            , color = Color.black
            , radius = relative <| 1 / 2
            , strokeWidth = relative <| 1 / 8
            , point = Point2d.pixels (width / 2) padding
            , direction =
                Direction2d.positiveX
            }
                |> BinarySigil.view

        Trump _ ->
            []

        Element _ ->
            { value = Card.value card - 1
            , size = 2
            , color = Color.black
            , radius = relative <| 1 / 2
            , strokeWidth = relative <| 1 / 8
            , point = Point2d.pixels (width / 2) padding
            , direction =
                Direction2d.positiveX
            }
                |> BinarySigil.view

        Planet _ ->
            { value = Card.value card - 1
            , size = 3
            , color = Color.black
            , radius = relative <| 1 / 2
            , strokeWidth = relative <| 1 / 8
            , point = Point2d.pixels (width / 2) padding
            , direction =
                Direction2d.positiveX
            }
                |> BinarySigil.view

        Binary n ->
            case n of
                1 ->
                    { value = n - 1
                    , size = 1
                    , color = "white"
                    , radius = relative <| 1 / 2
                    , strokeWidth = relative <| 1 / 8
                    , point = Point2d.pixels (width / 2) padding
                    , direction =
                        Direction2d.positiveX
                    }
                        |> BinarySigil.view

                2 ->
                    { value = n - 1
                    , size = 1
                    , color = Color.black
                    , radius = relative <| 1 / 2
                    , strokeWidth = relative <| 1 / 8
                    , point = Point2d.pixels (width / 2) padding
                    , direction =
                        Direction2d.positiveX
                    }
                        |> BinarySigil.view

                _ ->
                    []

        Virtue _ ->
            { value = Card.value card - 1
            , size = 4
            , color = Color.black
            , radius = relative <| 1 / 2
            , strokeWidth = relative <| 1 / 8
            , point = Point2d.pixels (width / 2) padding
            , direction =
                Direction2d.positiveX
            }
                |> BinarySigil.view

        _ ->
            []


view : Bool -> Card -> List (Svg msg)
view isGerman card =
    let
        isWhite =
            case card of
                Binary 1 ->
                    False

                Back ->
                    False

                _ ->
                    True

        { width, height, relative, radius } =
            defaultConfig
    in
    (Rectangle2d.from Point2d.origin (Point2d.pixels width height)
        |> Svg.rectangle2d
            [ Attributes.stroke "none"
            , Attributes.strokeWidth <| String.fromFloat <| 0
            , Attributes.fill <|
                if isWhite then
                    "white"

                else
                    Color.blackBackground
            ]
    )
        :: viewSigil defaultConfig card
        ++ (case card of
                Planet _ ->
                    []

                Element _ ->
                    [ Polyline2d.fromVertices
                        [ Point2d.pixels 0 (padding + radius)
                        , Point2d.pixels 0 0
                        , Point2d.pixels (padding + radius) 0
                        ]
                        |> Svg.polyline2d
                            [ Attributes.stroke <| "none"
                            , Attributes.strokeWidth <| String.fromFloat <| relative <| 0.1
                            , Attributes.fill Color.black
                            ]
                    , Polyline2d.fromVertices
                        [ Point2d.pixels (width - padding - radius) 0
                        , Point2d.pixels width 0
                        , Point2d.pixels width (padding + radius)
                        ]
                        |> Svg.polyline2d
                            [ Attributes.stroke <| "none"
                            , Attributes.strokeWidth <| String.fromFloat <| relative <| 0.1
                            , Attributes.fill Color.black
                            ]
                    , Polyline2d.fromVertices
                        [ Point2d.pixels width (height - padding - radius)
                        , Point2d.pixels width height
                        , Point2d.pixels (width - padding - radius) height
                        ]
                        |> Svg.polyline2d
                            [ Attributes.stroke <| "none"
                            , Attributes.strokeWidth <| String.fromFloat <| relative <| 0.1
                            , Attributes.fill Color.black
                            ]
                    , Polyline2d.fromVertices
                        [ Point2d.pixels (padding + radius) height
                        , Point2d.pixels 0 height
                        , Point2d.pixels 0 (height - padding - radius)
                        ]
                        |> Svg.polyline2d
                            [ Attributes.stroke <| "none"
                            , Attributes.strokeWidth <| String.fromFloat <| relative <| 0.1
                            , Attributes.fill Color.black
                            ]
                    ]

                Binary _ ->
                    [ Polyline2d.fromVertices
                        [ Point2d.pixels (12 * 7) (12 * 7)
                        , Point2d.pixels (12 * 7 + radius * 2) (12 * 7)
                        ]
                    , Polyline2d.fromVertices
                        [ Point2d.pixels (width - 12 * 7 - radius * 2) (12 * 7)
                        , Point2d.pixels (width - 12 * 7) (12 * 7)
                        ]
                    , Polyline2d.fromVertices
                        [ Point2d.pixels (width - 12 * 7) (12 * 7)
                        , Point2d.pixels (width - 12 * 7) (12 * 7 + radius * 2)
                        ]
                    , Polyline2d.fromVertices
                        [ Point2d.pixels (width - 12 * 7) (height - 12 * 7 - radius * 2)
                        , Point2d.pixels (width - 12 * 7) (height - 12 * 7)
                        ]
                    , Polyline2d.fromVertices
                        [ Point2d.pixels (width - 12 * 7) (height - 12 * 7)
                        , Point2d.pixels (width - 12 * 7 - radius * 2) (height - 12 * 7)
                        ]
                    , Polyline2d.fromVertices
                        [ Point2d.pixels (12 * 7 + radius * 2) (height - 12 * 7)
                        , Point2d.pixels (12 * 7) (height - 12 * 7)
                        ]
                    , Polyline2d.fromVertices
                        [ Point2d.pixels (12 * 7) (height - 12 * 7)
                        , Point2d.pixels (12 * 7) (height - 12 * 7 - radius * 2)
                        ]
                    , Polyline2d.fromVertices
                        [ Point2d.pixels (12 * 7) (12 * 7 + radius * 2)
                        , Point2d.pixels (12 * 7) (12 * 7)
                        ]
                    ]
                        |> List.map
                            (Svg.polyline2d
                                [ Attributes.stroke <| Card.color card
                                , Attributes.strokeWidth <| String.fromFloat <| relative <| 0.1
                                , Attributes.fill <| "none"
                                ]
                            )

                Trump _ ->
                    [ Polyline2d.fromVertices
                        [ Point2d.pixels (12 * 7) (12 * 7)
                        , Point2d.pixels (width - 12 * 7) (12 * 7)
                        ]
                    , Polyline2d.fromVertices
                        [ Point2d.pixels (width - 12 * 7) (height - 12 * 7)
                        , Point2d.pixels (12 * 7) (height - 12 * 7)
                        ]
                    , Polyline2d.fromVertices
                        [ Point2d.pixels (12 * 7) (height - 12 * 7)
                        , Point2d.pixels (12 * 7) (height - 12 * 7 - relative 1)
                        ]
                    , Polyline2d.fromVertices
                        [ Point2d.pixels (12 * 7) (12 * 7 + relative 1)
                        , Point2d.pixels (12 * 7) (12 * 7)
                        ]
                    , Polyline2d.fromVertices
                        [ Point2d.pixels (width - 12 * 7) (12 * 7)
                        , Point2d.pixels (width - 12 * 7) (12 * 7 + relative 1)
                        ]
                    , Polyline2d.fromVertices
                        [ Point2d.pixels (width - 12 * 7) (height - 12 * 7 - relative 1)
                        , Point2d.pixels (width - 12 * 7) (height - 12 * 7)
                        ]
                    ]
                        |> List.map
                            (Svg.polyline2d
                                [ Attributes.stroke <| Card.color card
                                , Attributes.strokeWidth <| String.fromFloat <| relative <| 0.1
                                , Attributes.fill <| "none"
                                , Attributes.strokeLinecap <| "square"
                                ]
                            )

                _ ->
                    Rectangle2d.from
                        (Point2d.pixels (12 * 7) (12 * 7))
                        (Point2d.pixels (width - 12 * 7) (height - 12 * 7))
                        |> Svg.rectangle2d
                            [ Attributes.stroke <| Card.color card
                            , Attributes.strokeWidth <| String.fromFloat <| relative <| 0.1
                            , Attributes.fill <| "none"
                            ]
                        |> List.singleton
           )
        ++ viewSymbol defaultConfig card
        ++ ((card
                |> Card.description isGerman
                |> Svg.text
                |> List.singleton
                |> Svg.text_
                    [ Attributes.x <| String.fromFloat <| width / 2
                    , Attributes.y <| String.fromFloat <| height - padding - relative 3
                    , Attributes.textAnchor <| "middle"
                    , Attributes.style <| "font: " ++ (String.fromFloat <| relative <| 3) ++ "px sans-serif"
                    , Attributes.fill <|
                        if isWhite then
                            Color.black

                        else
                            "white"
                    ]
            )
                :: (card
                        |> Card.title isGerman
                        |> Svg.text
                        |> List.singleton
                        |> Svg.text_
                            [ Attributes.x <| String.fromFloat <| width - padding - (relative <| 0.6)
                            , Attributes.y <| String.fromFloat <| padding
                            , Attributes.textAnchor <| "start"
                            , Attributes.style <| "font: " ++ (String.fromFloat <| relative <| 1.8) ++ "px sans-serif"
                            , Attributes.writingMode <| "tb"
                            , Attributes.fill <|
                                if isWhite then
                                    Color.black

                                else
                                    "white"
                            ]
                        |> List.singleton
                   )
           )
