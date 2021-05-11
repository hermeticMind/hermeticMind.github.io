module Data.Card exposing (Card(..), asList, back, color, description, title, value)

import View.Color as Color


type Card
    = Binary Int
    | Trump Int
    | Element Int
    | Planet Int
    | Virtue Int
    | Joker
    | Back


asList : List Card
asList =
    [ Binary 0, Binary 1, Joker ]
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
        Binary 1 ->
            "white"

        Binary 0 ->
            "black"

        Trump n ->
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
            ( "Narr", "Fool" )

        Binary n ->
            case n of
                0 ->
                    ( "Nacht", "Night" )

                1 ->
                    ( "Day", "Tag" )

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
                    ( "Das Rad des Schicksals", "Wheel of Fortune" )

                11 ->
                    ( "Die Kraft", "Strength" )

                12 ->
                    ( "Der Gehängte", "The Hanged Man" )

                13 ->
                    ( "Der Tod", "Death" )

                14 ->
                    ( "Die Mäßigkeit", "Temperance" )

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
                    ( "Mitgefühl", "Compassion" )

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
                    ( "Autentizität", "Authenticity" )

                11 ->
                    ( "Ehrlichkeit", "Honesty" )

                12 ->
                    ( "Mäßigkeit", "Temperance" )

                13 ->
                    ( "Humor", "Humor" )

                14 ->
                    ( "Hoffnung", "Hope" )

                15 ->
                    ( "Mut", "Courage" )

                16 ->
                    ( "Fleiß", "Diligence" )

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
            ( "Sorglosigkeit", "carefreeness" )

        Binary n ->
            case n of
                0 ->
                    ( "Geborgenheit", "Security" )

                1 ->
                    ( "Wandel", "Change" )

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
                    ( "Spiritualität", "Spirituality" )

                6 ->
                    ( "Verbindung", "Connection" )

                7 ->
                    ( "Erfolg", "Success" )

                8 ->
                    ( "Gerechtigkeit", "Justice" )

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
                    ( "Mäßigkeit", "Temperance" )

                15 ->
                    ( "Abhängigkeit", "Dependency" )

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
                    ( "Träumerei", "Reverie" )

                2 ->
                    ( "Emotion", "Emotion" )

                3 ->
                    ( "Realität", "Reality" )

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

        Virtue n ->
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
            0

        Binary n ->
            n

        Trump v ->
            v
                |> modBy 7
                |> (\n ->
                        if n == 0 then
                            7

                        else
                            n
                   )

        Element v ->
            v |> modBy 4

        Planet v ->
            v

        Virtue n ->
            n

        Back ->
            -1
