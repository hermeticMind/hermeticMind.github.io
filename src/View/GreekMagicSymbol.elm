module View.GreekMagicSymbol exposing (fromChar)

import Angle
import Arc2d
import Color exposing (Color)
import Data.Turtle as Turtle exposing (Turtle)
import Direction2d exposing (Direction2d)
import Geometry.Svg as Svg
import Html exposing (Html)
import LineSegment2d
import Point2d exposing (Point2d)
import Quantity exposing (Quantity(..))
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import Vector2d


zoom : number
zoom =
    1



--64


config : Float -> { eps : Float, unit : Float, size : Float, strokeWidth : Float }
config size =
    let
        unit =
            size / 9
    in
    { eps = 0.001
    , unit = unit
    , size = size
    , strokeWidth = unit
    }


overshoot =
    0.05


turtle strokeWidth position direction color =
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
                    [ Attributes.stroke color
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
                    , Attributes.stroke <| color
                    , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                    ]
            ]
    }


begin : Float -> Turtle (List (Svg msg)) -> ( Turtle (List (Svg msg)), List (Svg msg) )
begin strokeWidth =
    Turtle.rotateRightBy (Angle.radians <| pi / 2)
        >> Turtle.arcRightBy
            { angle = Angle.radians <| 0
            , radius = strokeWidth
            }
        >> Turtle.map (Turtle.rotateLeftBy (Angle.radians <| pi / 2))


end : Float -> ( Turtle (List (Svg msg)), List (Svg msg) ) -> List (Svg msg)
end strokeWidth =
    Turtle.map (Turtle.rotateRightBy (Angle.radians <| pi / 2))
        >> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| 0
                , radius = strokeWidth
                }
            )
        >> Turtle.map (Turtle.rotateLeftBy (Angle.radians <| pi / 2))
        >> Tuple.second


letterA : Float -> Turtle (List (Svg msg)) -> List (Svg msg)
letterA size t =
    let
        { eps, unit, strokeWidth } =
            config size
    in
    t
        |> Turtle.jumpBy (Vector2d.unsafe { x = -size / 2, y = -size / 2 })
        |> Turtle.jumpBy (Vector2d.unsafe { x = unit * 5 + strokeWidth / 2, y = strokeWidth + strokeWidth / 2 })
        |> Turtle.rotateTo Direction2d.negativeX
        |> begin strokeWidth
        |> Turtle.andThen (Turtle.forwardBy (unit * 1))
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi / 2
                , radius = eps
                }
            )
        |> Turtle.andThen (Turtle.forwardBy (3 * unit + strokeWidth))
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| 0
                , radius = unit * 2
                }
            )
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi / 2
                , radius = unit * 2
                }
            )
        |> end strokeWidth


letterB : Float -> Turtle (List (Svg msg)) -> List (Svg msg)
letterB size t =
    let
        { eps, unit, strokeWidth } =
            config size
    in
    t
        |> Turtle.jumpBy (Vector2d.unsafe { x = -size / 2, y = -size / 2 })
        |> Turtle.jumpBy (Vector2d.unsafe { x = unit * 2 + strokeWidth / 2, y = size - unit - strokeWidth / 2 })
        |> Turtle.rotateTo Direction2d.positiveX
        |> begin strokeWidth
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi / 2
                , radius = eps
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 6 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi / 2
                , radius = unit
                }
            )
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi
                , radius = unit
                }
            )
        |> Turtle.map (Turtle.rotateRightBy (Angle.radians <| pi))
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi / 2
                , radius = unit * 2
                }
            )
        |> end strokeWidth


letterC : Float -> Turtle (List (Svg msg)) -> List (Svg msg)
letterC size t =
    let
        { eps, unit, strokeWidth } =
            config size
    in
    t
        |> Turtle.jumpBy (Vector2d.unsafe { x = -size / 2, y = -size / 2 })
        |> Turtle.jumpBy (Vector2d.unsafe { x = unit * 2 + strokeWidth / 2, y = unit * 1 + strokeWidth / 2 })
        |> Turtle.rotateTo Direction2d.positiveX
        |> begin strokeWidth
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi / 2
                , radius = eps
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 6 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi * 3 / 2
                , radius = unit
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi / 2
                , radius = unit * 2
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 3 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi / 2
                , radius = eps
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| unit)
        |> end strokeWidth


letterD : Float -> Turtle (List (Svg msg)) -> List (Svg msg)
letterD size t =
    let
        { eps, unit, strokeWidth } =
            config size
    in
    t
        |> Turtle.jumpBy (Vector2d.unsafe { x = -size / 2, y = -size / 2 })
        |> Turtle.jumpBy (Vector2d.unsafe { x = unit * 5 + strokeWidth / 2, y = unit + strokeWidth / 2 })
        |> Turtle.rotateTo Direction2d.negativeX
        |> begin strokeWidth
        |> Turtle.andThen (Turtle.forwardBy <| unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi / 2
                , radius = eps
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 5 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi * 3 / 2
                , radius = unit * 2
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 4 * unit)
        |> end strokeWidth


letterE : Float -> Turtle (List (Svg msg)) -> List (Svg msg)
letterE size t =
    let
        { eps, unit, strokeWidth } =
            config size
    in
    t
        |> Turtle.jumpBy (Vector2d.unsafe { x = -size / 2, y = -size / 2 })
        |> Turtle.jumpBy (Vector2d.unsafe { x = unit * 3 + strokeWidth / 2, y = unit + strokeWidth / 2 })
        |> Turtle.rotateTo Direction2d.negativeX
        |> begin strokeWidth
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi / 2
                , radius = eps
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 3 * unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi * 3 / 2
                , radius = unit
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi / 2
                , radius = unit * 2
                }
            )
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi / 2
                , radius = unit * 2
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 4 * unit)
        |> end strokeWidth


letterF : Float -> Turtle (List (Svg msg)) -> List (Svg msg)
letterF size t =
    let
        { eps, unit, strokeWidth } =
            config size
    in
    t
        |> Turtle.jumpBy (Vector2d.unsafe { x = -size / 2, y = -size / 2 })
        |> Turtle.jumpBy (Vector2d.unsafe { x = unit * 2 + strokeWidth / 2, y = unit * 2 + strokeWidth / 2 })
        |> Turtle.rotateTo Direction2d.positiveX
        |> begin strokeWidth
        |> Turtle.andThen (Turtle.forwardBy <| 3 * unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi * 3 / 2
                , radius = unit
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 5 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi * 3 / 2
                , radius = unit * 2
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 4 * unit)
        |> end strokeWidth


letterG : Float -> Turtle (List (Svg msg)) -> List (Svg msg)
letterG size t =
    let
        { eps, unit, strokeWidth } =
            config size
    in
    t
        |> Turtle.jumpBy (Vector2d.unsafe { x = -size / 2, y = -size / 2 })
        |> Turtle.jumpBy (Vector2d.unsafe { x = unit * 5 + strokeWidth / 2, y = unit * 1 + strokeWidth / 2 })
        |> Turtle.rotateTo Direction2d.negativeX
        |> begin strokeWidth
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi / 2
                , radius = eps
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi
                , radius = unit * 2
                }
            )
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi
                , radius = unit
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 5 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi * 3 / 2
                , radius = unit
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 5 * unit)
        |> end strokeWidth


letterH : Float -> Turtle (List (Svg msg)) -> List (Svg msg)
letterH size t =
    let
        { eps, unit, strokeWidth } =
            config size
    in
    t
        |> Turtle.jumpBy (Vector2d.unsafe { x = -size / 2, y = -size / 2 })
        |> Turtle.jumpBy (Vector2d.unsafe { x = unit * 2 + strokeWidth / 2, y = unit * 6 + strokeWidth / 2 })
        |> Turtle.rotateTo Direction2d.positiveX
        |> begin strokeWidth
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi / 2
                , radius = eps
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi
                , radius = unit
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 6 * unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi
                , radius = unit
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 3 * unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi / 2
                , radius = unit
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 2 * unit)
        |> end strokeWidth


letterI : Float -> Turtle (List (Svg msg)) -> List (Svg msg)
letterI size t =
    let
        { eps, unit, strokeWidth } =
            config size
    in
    t
        |> Turtle.jumpBy (Vector2d.unsafe { x = -size / 2, y = -size / 2 })
        |> Turtle.jumpBy (Vector2d.unsafe { x = unit * 2 + strokeWidth / 2, y = unit * 1 + strokeWidth / 2 })
        |> Turtle.rotateTo Direction2d.positiveX
        |> begin strokeWidth
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi / 2
                , radius = eps
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 3 * unit)
        |> Turtle.map (Turtle.rotateRightBy (Angle.radians <| pi))
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi
                , radius = unit
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 3 * unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi / 2
                , radius = unit
                }
            )
        |> end strokeWidth


letterJ : Float -> Turtle (List (Svg msg)) -> List (Svg msg)
letterJ size t =
    let
        { eps, unit, strokeWidth } =
            config size
    in
    t
        |> Turtle.jumpBy (Vector2d.unsafe { x = -size / 2, y = -size / 2 })
        |> Turtle.jumpBy (Vector2d.unsafe { x = unit * 2 + strokeWidth / 2, y = unit * 1 + strokeWidth / 2 })
        |> Turtle.rotateTo Direction2d.positiveX
        |> begin strokeWidth
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi / 2
                , radius = eps
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 4 * unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi / 2
                , radius = unit * 2
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> end strokeWidth


letterK : Float -> Turtle (List (Svg msg)) -> List (Svg msg)
letterK size t =
    let
        { eps, unit, strokeWidth } =
            config size
    in
    t
        |> Turtle.jumpBy (Vector2d.unsafe { x = -size / 2, y = -size / 2 })
        |> Turtle.jumpBy (Vector2d.unsafe { x = unit * 2 + strokeWidth / 2, y = unit * 1 + strokeWidth / 2 })
        |> Turtle.rotateTo Direction2d.positiveX
        |> begin strokeWidth
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi / 2
                , radius = eps
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 6 * unit)
        |> Turtle.map (Turtle.rotateRightBy (Angle.radians <| pi))
        |> Turtle.andThen (Turtle.forwardBy <| 4 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi * 3 / 2
                , radius = unit
                }
            )
        |> Turtle.map (Turtle.rotateRightBy (Angle.radians <| pi))
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi / 2
                , radius = unit
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi / 2
                , radius = unit
                }
            )
        |> end strokeWidth


letterL : Float -> Turtle (List (Svg msg)) -> List (Svg msg)
letterL size t =
    let
        { eps, unit, strokeWidth } =
            config size
    in
    t
        |> Turtle.jumpBy (Vector2d.unsafe { x = -size / 2, y = -size / 2 })
        |> Turtle.jumpBy (Vector2d.unsafe { x = unit * 2 + strokeWidth / 2, y = unit * 1 + strokeWidth / 2 })
        |> Turtle.rotateTo Direction2d.positiveX
        |> begin strokeWidth
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi / 2
                , radius = eps
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 5 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi
                , radius = unit
                }
            )
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi
                , radius = unit * 2
                }
            )
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi / 2
                , radius = unit
                }
            )
        |> end strokeWidth


letterM : Float -> Turtle (List (Svg msg)) -> List (Svg msg)
letterM size t =
    let
        { eps, unit, strokeWidth } =
            config size
    in
    t
        |> Turtle.jumpBy (Vector2d.unsafe { x = -size / 2, y = -size / 2 })
        |> Turtle.jumpBy (Vector2d.unsafe { x = unit * 2 + strokeWidth / 2, y = unit * 7 + strokeWidth / 2 })
        |> Turtle.rotateTo Direction2d.positiveX
        |> begin strokeWidth
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi / 2
                , radius = eps
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 7 * unit + strokeWidth / 2)
        |> Turtle.map (Turtle.rotateRightBy (Angle.radians <| pi))
        |> Turtle.andThen (Turtle.forwardBy <| 2 * unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi
                , radius = unit
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 2 * unit)
        |> Turtle.map (Turtle.rotateRightBy (Angle.radians <| pi))
        |> Turtle.andThen (Turtle.forwardBy <| 2 * unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi / 2
                , radius = unit
                }
            )
        |> end strokeWidth


letterN : Float -> Turtle (List (Svg msg)) -> List (Svg msg)
letterN size t =
    let
        { eps, unit, strokeWidth } =
            config size
    in
    t
        |> Turtle.jumpBy (Vector2d.unsafe { x = -size / 2, y = -size / 2 })
        |> Turtle.jumpBy (Vector2d.unsafe { x = unit * 1 + strokeWidth / 2, y = unit * 4 + strokeWidth / 2 })
        |> Turtle.rotateTo Direction2d.positiveY
        |> begin strokeWidth
        |> Turtle.andThen (Turtle.forwardBy <| 2 * unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi
                , radius = unit * 2
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 5 * unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi * 3 / 2
                , radius = unit
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 2 * unit)
        |> end strokeWidth


letterO : Float -> Turtle (List (Svg msg)) -> List (Svg msg)
letterO size t =
    let
        { eps, unit, strokeWidth } =
            config size
    in
    t
        |> Turtle.jumpBy (Vector2d.unsafe { x = -size / 2, y = -size / 2 })
        |> Turtle.jumpBy (Vector2d.unsafe { x = unit * 4 + strokeWidth / 2, y = unit * 2 + strokeWidth / 2 })
        |> Turtle.rotateTo Direction2d.positiveY
        |> begin strokeWidth
        |> Turtle.andThen (Turtle.forwardBy <| 2 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi / 2
                , radius = eps
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 2 * unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi * 3 / 2
                , radius = unit * 2
                }
            )
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi / 2
                , radius = unit
                }
            )
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi
                , radius = unit
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 3 * unit)
        |> end strokeWidth


letterP : Float -> Turtle (List (Svg msg)) -> List (Svg msg)
letterP size t =
    let
        { eps, unit, strokeWidth } =
            config size
    in
    t
        |> Turtle.jumpBy (Vector2d.unsafe { x = -size / 2, y = -size / 2 })
        |> Turtle.jumpBy (Vector2d.unsafe { x = unit * 2 + strokeWidth / 2, y = unit * 2 + strokeWidth / 2 })
        |> Turtle.rotateTo Direction2d.positiveY
        |> begin strokeWidth
        |> Turtle.andThen (Turtle.forwardBy <| 5 * unit)
        |> Turtle.map (Turtle.rotateRightBy (Angle.radians <| pi))
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi
                , radius = unit
                }
            )
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi * 3 / 2
                , radius = unit * 2
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 4 * unit)
        |> end strokeWidth


letterQ : Float -> Turtle (List (Svg msg)) -> List (Svg msg)
letterQ size t =
    let
        { eps, unit, strokeWidth } =
            config size
    in
    t
        |> Turtle.jumpBy (Vector2d.unsafe { x = -size / 2, y = -size / 2 })
        |> Turtle.jumpBy (Vector2d.unsafe { x = unit * 1 + strokeWidth / 2, y = unit * 2 + strokeWidth / 2 })
        |> Turtle.rotateTo Direction2d.positiveY
        |> begin strokeWidth
        |> Turtle.andThen (Turtle.forwardBy <| 4 * unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi * 3 / 2
                , radius = unit * 2
                }
            )
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi / 2
                , radius = unit
                }
            )
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi / 2
                , radius = unit * 2
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 2 * unit)
        |> end strokeWidth


letterR : Float -> Turtle (List (Svg msg)) -> List (Svg msg)
letterR size t =
    let
        { eps, unit, strokeWidth } =
            config size
    in
    t
        |> Turtle.jumpBy (Vector2d.unsafe { x = -size / 2, y = -size / 2 })
        |> Turtle.jumpBy (Vector2d.unsafe { x = unit * 2 + strokeWidth / 2, y = unit * 1 + strokeWidth / 2 })
        |> Turtle.rotateTo Direction2d.positiveX
        |> begin strokeWidth
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi / 2
                , radius = eps
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi
                , radius = unit * 2.5
                }
            )
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi
                , radius = unit
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 4 * unit)
        |> end strokeWidth


letterS : Float -> Turtle (List (Svg msg)) -> List (Svg msg)
letterS size t =
    let
        { eps, unit, strokeWidth } =
            config size
    in
    t
        |> Turtle.jumpBy (Vector2d.unsafe { x = -size / 2, y = -size / 2 })
        |> Turtle.jumpBy (Vector2d.unsafe { x = unit * 5 + strokeWidth / 2, y = unit * 1 + strokeWidth / 2 })
        |> Turtle.rotateTo Direction2d.negativeX
        |> begin strokeWidth
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi / 2
                , radius = unit * 2
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi / 2
                , radius = unit
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 3 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi
                , radius = unit
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 5 * unit)
        |> end strokeWidth


letterT : Float -> Turtle (List (Svg msg)) -> List (Svg msg)
letterT size t =
    let
        { eps, unit, strokeWidth } =
            config size
    in
    t
        |> Turtle.jumpBy (Vector2d.unsafe { x = -size / 2, y = -size / 2 })
        |> Turtle.jumpBy (Vector2d.unsafe { x = unit * 3 + strokeWidth / 2, y = unit * 1 + strokeWidth / 2 })
        |> Turtle.rotateTo Direction2d.positiveX
        |> begin strokeWidth
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi / 2
                , radius = eps
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 5 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi
                , radius = unit * 2
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi / 2
                , radius = unit * 2
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 4 * unit)
        |> end strokeWidth


letterU : Float -> Turtle (List (Svg msg)) -> List (Svg msg)
letterU size t =
    let
        { eps, unit, strokeWidth } =
            config size
    in
    t
        |> Turtle.jumpBy (Vector2d.unsafe { x = -size / 2, y = -size / 2 })
        |> Turtle.jumpBy (Vector2d.unsafe { x = unit * 2 + strokeWidth / 2, y = unit * 1 + strokeWidth / 2 })
        |> Turtle.rotateTo Direction2d.positiveX
        |> begin strokeWidth
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi / 2
                , radius = eps
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 5 * unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi
                , radius = unit * 2
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 4 * unit)
        |> end strokeWidth


letterV : Float -> Turtle (List (Svg msg)) -> List (Svg msg)
letterV size t =
    let
        { eps, unit, strokeWidth } =
            config size
    in
    t
        |> Turtle.jumpBy (Vector2d.unsafe { x = -size / 2, y = -size / 2 })
        |> Turtle.jumpBy (Vector2d.unsafe { x = unit * 2 + strokeWidth / 2, y = unit * 4 + strokeWidth / 2 })
        |> Turtle.rotateTo Direction2d.positiveX
        |> begin strokeWidth
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi / 2
                , radius = eps
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 1.5 * unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi
                , radius = unit * 2.5
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 0.5 * unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi / 2
                , radius = unit * 4
                }
            )
        |> end strokeWidth


letterW : Float -> Turtle (List (Svg msg)) -> List (Svg msg)
letterW size t =
    let
        { eps, unit, strokeWidth } =
            config size
    in
    t
        |> Turtle.jumpBy (Vector2d.unsafe { x = -size / 2, y = -size / 2 })
        |> Turtle.jumpBy (Vector2d.unsafe { x = unit * 2 + strokeWidth / 2, y = unit * 4 + strokeWidth / 2 })
        |> Turtle.rotateTo Direction2d.positiveX
        |> begin strokeWidth
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi / 2
                , radius = eps
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 2.25 * unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi
                , radius = unit * 1.75
                }
            )
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi
                , radius = unit
                }
            )
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi
                , radius = unit * 1.75
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 1.25 * unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi / 2
                , radius = unit * 4
                }
            )
        |> end strokeWidth


letterX : Float -> Turtle (List (Svg msg)) -> List (Svg msg)
letterX size t =
    let
        { eps, unit, strokeWidth } =
            config size
    in
    t
        |> Turtle.jumpBy (Vector2d.unsafe { x = -size / 2, y = -size / 2 })
        |> Turtle.jumpBy (Vector2d.unsafe { x = unit * 2 + strokeWidth / 2, y = unit * 1 + strokeWidth / 2 })
        |> Turtle.rotateTo Direction2d.positiveX
        |> begin strokeWidth
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi / 2
                , radius = eps
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 6 * unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi
                , radius = unit * 1
                }
            )
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi
                , radius = unit * 2.5
                }
            )
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi
                , radius = unit * 1
                }
            )
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi / 2
                , radius = unit * 4
                }
            )
        |> end strokeWidth


letterY : Float -> Turtle (List (Svg msg)) -> List (Svg msg)
letterY size t =
    let
        { eps, unit, strokeWidth } =
            config size
    in
    t
        |> Turtle.jumpBy (Vector2d.unsafe { x = -size / 2, y = -size / 2 })
        |> Turtle.jumpBy (Vector2d.unsafe { x = unit * 2 + strokeWidth / 2, y = unit * 1 + strokeWidth / 2 })
        |> Turtle.rotateTo Direction2d.positiveX
        |> begin strokeWidth
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi / 2
                , radius = eps
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 7 * unit + strokeWidth / 2)
        |> Turtle.map (Turtle.rotateRightBy (Angle.radians <| pi))
        |> Turtle.andThen (Turtle.forwardBy <| 2 * unit + strokeWidth / 2)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi / 2
                , radius = unit
                }
            )
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi / 2
                , radius = unit * 3
                }
            )
        |> end strokeWidth


letterZ : Float -> Turtle (List (Svg msg)) -> List (Svg msg)
letterZ size t =
    let
        { eps, unit, strokeWidth } =
            config size
    in
    t
        |> Turtle.jumpBy (Vector2d.unsafe { x = -size / 2, y = -size / 2 })
        |> Turtle.jumpBy (Vector2d.unsafe { x = unit * 2 + strokeWidth / 2, y = unit * 1 + strokeWidth / 2 })
        |> Turtle.rotateTo Direction2d.positiveX
        |> begin strokeWidth
        |> Turtle.andThen (Turtle.forwardBy <| 3 * unit)
        |> Turtle.map (Turtle.rotateRightBy (Angle.radians <| pi))
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi / 2
                , radius = unit * 2
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi / 2
                , radius = unit
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 3 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi
                , radius = unit
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 3 * unit)
        |> end strokeWidth


fromChar : { size : Float, position : Point2d Float (), direction : Direction2d (), color : String } -> Char -> Maybe (List (Svg msg))
fromChar { size, position, direction, color } char =
    let
        { eps, unit, strokeWidth } =
            config size

        t =
            turtle strokeWidth position direction color

        fun letter =
            letter size t
                |> Just
    in
    case char |> Char.toLower of
        'a' ->
            fun letterA

        'b' ->
            fun letterB

        'c' ->
            fun letterC

        'd' ->
            fun letterD

        'e' ->
            fun letterE

        'f' ->
            fun letterF

        'g' ->
            fun letterG

        'h' ->
            fun letterH

        'i' ->
            fun letterI

        'j' ->
            fun letterJ

        'k' ->
            fun letterK

        'l' ->
            fun letterL

        'm' ->
            fun letterM

        'n' ->
            fun letterN

        'o' ->
            fun letterO

        'p' ->
            fun letterP

        'q' ->
            fun letterQ

        'r' ->
            fun letterR

        's' ->
            fun letterS

        't' ->
            fun letterT

        'u' ->
            fun letterU

        'v' ->
            fun letterV

        'w' ->
            fun letterW

        'x' ->
            fun letterX

        'y' ->
            fun letterY

        'z' ->
            fun letterZ

        _ ->
            Nothing
