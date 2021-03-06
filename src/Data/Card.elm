module Data.Card exposing (Card(..), asList, back, color, decode, description, encode, fromSigil, parser, title, value)

import Parser exposing ((|.), (|=), Parser)
import View.Color as Color


type Card
    = Binary Int
    | Trump Int
    | Element Int
    | Planet Int
    | Virtue Int
    | Joker
    | Back


fromSigil :
    { value : Int
    , size : Int
    }
    -> Card
fromSigil config =
    config.value
        + 1
        |> (case config.size of
                0 ->
                    always Joker

                1 ->
                    Binary

                2 ->
                    Element

                3 ->
                    Planet

                4 ->
                    Virtue

                _ ->
                    always Back
           )


encode : Card -> String
encode card =
    case card of
        Binary int ->
            "binary " ++ String.fromInt int

        Trump int ->
            "trump " ++ String.fromInt int

        Element int ->
            "element " ++ String.fromInt int

        Planet int ->
            "planet " ++ String.fromInt int

        Virtue int ->
            "virtue " ++ String.fromInt int

        Joker ->
            "joker"

        Back ->
            "back"


parser : Parser Card
parser =
    Parser.oneOf
        [ Parser.succeed Binary
            |. Parser.keyword "binary"
            |. Parser.spaces
            |= Parser.int
        , Parser.succeed Trump
            |. Parser.keyword "trump"
            |. Parser.spaces
            |= Parser.int
        , Parser.succeed Element
            |. Parser.keyword "element"
            |. Parser.spaces
            |= Parser.int
        , Parser.succeed Planet
            |. Parser.keyword "planet"
            |. Parser.spaces
            |= Parser.int
        , Parser.succeed Virtue
            |. Parser.keyword "virtue"
            |. Parser.spaces
            |= Parser.int
        , Parser.succeed Joker
            |. Parser.keyword "joker"
        , Parser.succeed Back
            |. Parser.keyword "back"
        ]


decode : String -> Result () Card
decode =
    Parser.run parser
        >> Result.mapError (always ())


asList : List Card
asList =
    [ Binary 1, Binary 2, Joker ]
        |> List.append
            (List.range 1 16
                |> List.map Virtue
            )
        |> List.append
            ([ 1, 2, 5, 8, 9, 10, 12, 15, 18 ]
                |> List.map Trump
            )
        |> List.append
            ([ Element ]
                |> List.concatMap (\fun -> List.range 1 4 |> List.map fun)
            )
        |> List.append
            ([ Planet ]
                |> List.concatMap (\fun -> List.range 1 8 |> List.map fun)
            )


back : Card
back =
    Back


color : Card -> String
color card =
    case card of
        Binary 2 ->
            "black"

        Binary 1 ->
            "white"

        Trump _ ->
            Color.gray

        Planet _ ->
            "black"

        Element _ ->
            "black"

        Virtue n ->
            if n <= 4 then
                Color.blue

            else if n <= 8 then
                Color.red

            else if n <= 12 then
                Color.green

            else
                Color.yellow

        _ ->
            "none"


title : Bool -> Card -> String
title isGerman card =
    let
        default =
            ( "", "" )
    in
    (case card of
        Joker ->
            ( "Narr", "The Fool" )

        Binary n ->
            case n of
                1 ->
                    ( "Nacht", "Night" )

                2 ->
                    ( "Tag", "Day" )

                _ ->
                    default

        Trump n ->
            case n of
                1 ->
                    ( "Der Magier", "The Magician" )

                2 ->
                    ( "Die Hohepriesterin", "The High Priestess" )

                3 ->
                    ( "Die Herrscherin", "The Empress" )

                4 ->
                    ( "Der Herrscher", "The Emperor" )

                5 ->
                    ( "Der Hierophant", "The Hierophant" )

                6 ->
                    ( "Die Liebenden", "The Lovers" )

                7 ->
                    ( "Der Wagen", "The Chariot" )

                8 ->
                    ( "Die Gerechtigkeit", "Justice" )

                9 ->
                    ( "Der Eremit", "The Hermit" )

                10 ->
                    ( "Das Rad des Schicksals", "The Wheel of Fortune" )

                11 ->
                    ( "Die Kraft", "Strength" )

                12 ->
                    ( "Der Geh??ngte", "The Hanged Man" )

                13 ->
                    ( "Der Tod", "Death" )

                14 ->
                    ( "Die M????igkeit", "Temperance" )

                15 ->
                    ( "Der Teufel", "The Devil" )

                16 ->
                    ( "Der Turm", "The Tower" )

                17 ->
                    ( "Der Stern", "The Star" )

                18 ->
                    ( "Der Mond", "The Moon" )

                19 ->
                    ( "Die Sonne", "The Sun" )

                20 ->
                    ( "Das Gericht", "Judgement" )

                21 ->
                    ( "Die Welt", "The World" )

                _ ->
                    default

        Element n ->
            case n of
                1 ->
                    ( "Erde", "Earth" )

                2 ->
                    ( "Feuer", "Fire" )

                3 ->
                    ( "Luft", "Air" )

                4 ->
                    ( "Wasser", "Water" )

                _ ->
                    default

        Planet n ->
            case n of
                1 ->
                    ( "Merkur", "Mercury" )

                2 ->
                    ( "Venus", "Venus" )

                3 ->
                    ( "Planet Erde", "Planet Earth" )

                4 ->
                    ( "Mars", "Mars" )

                5 ->
                    ( "Jupiter", "Jupiter" )

                6 ->
                    ( "Saturn", "Saturn" )

                7 ->
                    ( "Uranus", "Uranus" )

                8 ->
                    ( "Neptun", "Neptun" )

                _ ->
                    default

        Virtue n ->
            case n of
                1 ->
                    ( "Mitgef??hl", "Compassion" )

                2 ->
                    ( "Freundlichkeit", "Kindness" )

                3 ->
                    ( "Offenheit", "Openness" )

                4 ->
                    ( "Vergebung", "Forgiveness" )

                5 ->
                    ( "Geduld", "Patience" )

                6 ->
                    ( "Treue", "Loyalty" )

                7 ->
                    ( "Selbstbeherrschung", "Self-control" )

                8 ->
                    ( "Ausdauer", "Endurance" )

                9 ->
                    ( "Selbsterkenntnis", "Self-knowledge" )

                10 ->
                    ( "Autentizit??t", "Authenticity" )

                11 ->
                    ( "Ehrlichkeit", "Honesty" )

                12 ->
                    ( "M????igkeit", "Temperance" )

                13 ->
                    ( "Humor", "Humor" )

                14 ->
                    ( "Hoffnung", "Hope" )

                15 ->
                    ( "Mut", "Courage" )

                16 ->
                    ( "Flei??", "Diligence" )

                _ ->
                    default

        Back ->
            default
    )
        |> (if isGerman then
                Tuple.first

            else
                Tuple.second
           )


description : Bool -> Card -> String
description isGerman card =
    let
        default =
            ( "", "" )
    in
    (case card of
        Joker ->
            ( "Sorglosigkeit", "Carefreeness" )

        Binary n ->
            case n of
                1 ->
                    ( "Wandel", "Change" )

                2 ->
                    ( "Geborgenheit", "Protection" )

                _ ->
                    default

        Trump n ->
            case n of
                1 ->
                    ( "Klarheit", "Clarity" )

                2 ->
                    ( "Intuition", "Intuition" )

                3 ->
                    ( "Selbstvertrauen", "Self-confidence" )

                4 ->
                    ( "Selbstbeherrschung", "Self-control" )

                5 ->
                    ( "Geist", "Spirit" )

                6 ->
                    ( "Verbindung", "Connection" )

                7 ->
                    ( "Erfolg", "Success" )

                8 ->
                    ( "Entscheidung", "Decision" )

                9 ->
                    ( "Suche", "Search" )

                10 ->
                    ( "Schicksal", "Fate" )

                11 ->
                    ( "Kraft", "Strength" )

                12 ->
                    ( "Ruhe", "Calmness" )

                13 ->
                    ( "Wandel", "Change" )

                14 ->
                    ( "M????igkeit", "Temperance" )

                15 ->
                    ( "Abh??ngigkeit", "Dependency" )

                16 ->
                    ( "Konfrontation", "Confrontation" )

                17 ->
                    ( "Hoffnung", "Hope" )

                18 ->
                    ( "Hingabe", "Dedication" )

                19 ->
                    ( "Zufriedenheit", "Satisfaction" )

                20 ->
                    ( "Neubeginn", "Restart" )

                21 ->
                    ( "Selbsterkenntnis", "Self-knowledge" )

                _ ->
                    default

        Element n ->
            case n of
                1 ->
                    ( "Tr??umerei", "Reverie" )

                2 ->
                    ( "Emotion", "Emotion" )

                3 ->
                    ( "Realit??t", "Reality" )

                4 ->
                    ( "Taktik", "Tactics" )

                _ ->
                    default

        Planet n ->
            case n of
                1 ->
                    ( "Wissen", "Knowledge" )

                2 ->
                    ( "Familie/Liebe", "Family/Love" )

                3 ->
                    ( "Gesundheit", "Health" )

                4 ->
                    ( "Reichtum", "Wealth" )

                5 ->
                    ( "Verwirklichung", "Fulfillment" )

                6 ->
                    ( "Anerkennung", "Acknowledgment" )

                7 ->
                    ( "Erfolg", "Success" )

                8 ->
                    ( "Freiheit", "Freedom" )

                _ ->
                    default

        Virtue _ ->
            default

        Back ->
            default
    )
        |> (case card of
                Virtue _ ->
                    always <| title isGerman card

                _ ->
                    if isGerman then
                        Tuple.first

                    else
                        Tuple.second
           )


value : Card -> Int
value card =
    case card of
        Joker ->
            1

        Binary n ->
            n

        Trump n ->
            n

        Element v ->
            v |> modBy 4 |> (+) 1

        Planet v ->
            v

        Virtue n ->
            n

        Back ->
            -1
