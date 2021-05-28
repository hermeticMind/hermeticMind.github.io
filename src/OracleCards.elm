module OracleCards exposing (main)

import Data.Card as Card exposing (Card(..))
import Geometry.Svg as Svg
import Html exposing (Html)
import Svg
import Svg.Attributes as Attributes
import View.Card as Card


symbolOnly : Bool
symbolOnly =
    False


main : Html msg
main =
    let
        zoom : Float
        zoom =
            --4 * 0.63
            --0.63
            --0.73
            0.36

        { width, height } =
            if symbolOnly then
                let
                    size : Float
                    size =
                        640

                    --Card.defaultConfig.width
                in
                { width = size, height = size }

            else
                { width = Card.defaultConfig.width
                , height = Card.defaultConfig.height
                }
    in
    Back
        :: Card.asList
        |> List.map
            (\card ->
                (if symbolOnly then
                    Card.viewSymbol (Card.defaultConfig |> (\config -> { config | width = width, height = height })) card

                 else
                    Card.view False card
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
            )
        |> Html.div []
