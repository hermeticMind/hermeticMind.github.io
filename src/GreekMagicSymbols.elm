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
import View.GreekMagicSymbol as GreekMagicSymbol


main : Html msg
main =
    let
        size =
            9
    in
    Alphabet.asList
        |> List.filterMap (GreekMagicSymbol.fromChar size)
        |> Html.div []
