module GreekMagicSymbols exposing (main)

import Angle
import Arc2d
import Data.Alphabet as Alphabet
import Data.Turtle as Turtle exposing (Turtle)
import Direction2d
import Geometry.Svg as Svg
import Html exposing (Html)
import LineSegment2d
import Point2d
import Quantity exposing (Quantity(..))
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import Vector2d
import View.Color as Color
import View.GreekMagicSymbol as GreekMagicSymbol


main : Html msg
main =
    let
        size =
            9

        zoom =
            3
    in
    Alphabet.asList
        |> List.filterMap
            (GreekMagicSymbol.fromChar
                { size = size
                , position = Point2d.unsafe { x = size / 2, y = size / 2 }
                , direction = Direction2d.positiveX
                , color = Color.primary
                }
                >> Maybe.map
                    (Svg.svg
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
