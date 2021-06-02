module BinarySigil exposing (..)

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
import View.Color as Color


zoom : Float
zoom =
    6


size =
    10


ringWidth =
    5


main : Html msg
main =
    List.range 1 4
        |> List.concatMap
            (\n ->
                List.range 0 ((2 ^ n) - 1)
                    |> List.map
                        (\r ->
                            { value = r
                            , size = n
                            , color = Color.primary --"black"
                            , radius = 1 / 2
                            , strokeWidth = 1 / 4 --1 / 8
                            , point = Point2d.pixels (size / 2) (size / 2)
                            , direction = Direction2d.positiveX
                            }
                                |> Sigil.view
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
                        )
            )
        |> Html.div []
