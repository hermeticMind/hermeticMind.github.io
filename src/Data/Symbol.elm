module Data.Symbol exposing (Symbol(..), meaning)


type Symbol
    = Circle
    | Triangle
    | Square


meaning : Bool -> Symbol -> String
meaning isGerman symbol =
    case symbol of
        Circle ->
            if isGerman then
                "intellektuelle und geistliche Aspekte"

            else
                "the mind"

        Triangle ->
            if isGerman then
                "soziale Interaktionen"

            else
                "social interactions"

        Square ->
            if isGerman then
                "materielle Aspekte"

            else
                "material matters"
