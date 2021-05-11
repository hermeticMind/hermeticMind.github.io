module OracleCards exposing (main)

import Data.Card as Card exposing (Card(..))
import Geometry.Svg as Svg
import Html exposing (Html)
import Svg
import Svg.Attributes as Attributes
import View.Card as Card


main : Html msg
main =
    Back
        :: Card.asList
        |> List.map
            (\card ->
                Card.view True card
                    |> Svg.svg
                        [ Attributes.width <| (String.fromFloat <| Card.zoom * Card.width) ++ "px"
                        , Attributes.height <| (String.fromFloat <| Card.zoom * Card.height) ++ "px"
                        , Attributes.version <| "1.1"
                        , Attributes.viewBox <|
                            "0 0 "
                                ++ String.fromFloat Card.width
                                ++ " "
                                ++ String.fromFloat Card.height
                        ]
            )
        |> Html.div []



{--
--}
