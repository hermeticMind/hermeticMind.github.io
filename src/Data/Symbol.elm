module Data.Symbol exposing (Symbol(..), meaning)


type Symbol
    = Circle
    | Triangle
    | Square


meaning : Symbol -> String
meaning symbol =
    case symbol of
        Circle ->
            "the mind"

        Triangle ->
            "social interactions"

        Square ->
            "material matters"
