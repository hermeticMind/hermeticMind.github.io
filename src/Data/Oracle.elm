module Data.Oracle exposing (reading)

import Data.Card as Card exposing (Card(..))
import Data.Card.Interpretation as Interpretation
import Data.Symbol as Symbol exposing (Symbol(..))
import Html.Attributes exposing (list)
import List.Extra as List
import String.Extra as String


reading : String -> ( Card, Card, Card ) -> String
reading string cards =
    let
        isGerman =
            False
    in
    "<page>"
        ++ "# Spread Report\n "
        ++ "For this reading we are using three cards. "
        ++ "The visual, semantical and numerological similarities between the cards are used for the interpretation.\n"
        ++ sectionYourCards isGerman cards
        ++ "## Summary\n"
        ++ (string ++ "\n")
        ++ sectionAspects isGerman cards
        ++ "</page><page>"
        ++ sectionNumerology isGerman cards
        ++ "</page>"


findSymbolism : List Card -> ( Maybe ( Symbol, List Card ), Maybe ( Symbol, List Card ) )
findSymbolism =
    List.map (\c -> ( c, Interpretation.fromCard c ))
        >> (\list ->
                let
                    primary : Maybe ( Symbol, List Card )
                    primary =
                        let
                            ( circle, rest ) =
                                list |> List.partition (\( _, ( a, _ ) ) -> a.symbol == Circle)

                            ( square, triangle ) =
                                rest |> List.partition (\( _, ( a, _ ) ) -> a.symbol == Square)
                        in
                        if ((circle |> List.length) > (square |> List.length)) && ((circle |> List.length) > (triangle |> List.length)) then
                            Just ( Circle, circle |> List.map Tuple.first )

                        else if (square |> List.length) > (circle |> List.length) && (square |> List.length) > (triangle |> List.length) then
                            Just ( Square, square |> List.map Tuple.first )

                        else if (triangle |> List.length) > (circle |> List.length) && (triangle |> List.length) > (square |> List.length) then
                            Just ( Triangle, triangle |> List.map Tuple.first )

                        else
                            Nothing

                    secondary : Maybe ( Symbol, List Card )
                    secondary =
                        let
                            primarySymbol : Maybe Symbol
                            primarySymbol =
                                primary
                                    |> Maybe.map Tuple.first

                            circle =
                                if primarySymbol == Just Circle then
                                    []

                                else
                                    list
                                        |> List.filter
                                            (\( _, ( a, b ) ) ->
                                                (a.symbol == Circle)
                                                    || (b |> Maybe.map (.symbol >> (==) Circle) |> Maybe.withDefault False)
                                            )
                                        |> Debug.log "circle"

                            square =
                                if primarySymbol == Just Square then
                                    []

                                else
                                    list
                                        |> List.filter
                                            (\( _, ( a, b ) ) ->
                                                (a.symbol == Square)
                                                    || (b |> Maybe.map (.symbol >> (==) Square) |> Maybe.withDefault False)
                                            )
                                        |> Debug.log "square"

                            triangle =
                                if primarySymbol == Just Triangle then
                                    []

                                else
                                    list
                                        |> List.filter
                                            (\( _, ( a, b ) ) ->
                                                (a.symbol == Triangle)
                                                    || (b |> Maybe.map (.symbol >> (==) Triangle) |> Maybe.withDefault False)
                                            )
                                        |> Debug.log "triangle"
                        in
                        if
                            ((circle |> List.length) > (square |> List.length))
                                && ((circle |> List.length) > (triangle |> List.length))
                        then
                            Just ( Circle, circle |> List.map Tuple.first )

                        else if
                            ((square |> List.length) > (circle |> List.length))
                                && ((square |> List.length) > (triangle |> List.length))
                        then
                            Just ( Square, square |> List.map Tuple.first )

                        else if
                            ((triangle |> List.length) > (circle |> List.length))
                                && ((triangle |> List.length) > (square |> List.length))
                        then
                            Just ( Triangle, triangle |> List.map Tuple.first )

                        else
                            primarySymbol
                                |> Maybe.andThen
                                    (\s ->
                                        list
                                            |> List.find (Tuple.second >> Tuple.first >> .symbol >> (/=) s)
                                            |> Maybe.map (\( c, ( { symbol }, _ ) ) -> ( symbol, [ c ] ))
                                    )
                in
                ( primary, secondary )
           )


sectionYourCards : Bool -> ( Card, Card, Card ) -> String
sectionYourCards isGerman ( c1, c2, c3 ) =
    let
        list =
            [ c1, c2, c3 ]

        ( primary, secondary ) =
            list
                |> findSymbolism
    in
    "<interactive name=\"Oracle\" value=\"("
        ++ (c1 |> Card.encode)
        ++ ","
        ++ (c2 |> Card.encode)
        ++ ","
        ++ (c3 |> Card.encode)
        ++ ")\">"
        ++ (case primary of
                Just ( s1, l1 ) ->
                    (case l1 of
                        [] ->
                            ""

                        [ head ] ->
                            (head |> Card.description isGerman >> (\t -> "_" ++ t ++ "_"))
                                ++ "refers to "
                                ++ (s1 |> Symbol.meaning)
                                ++ "."

                        [ _, _, _ ] ->
                            "All three cards should be read together and refer to "
                                ++ (s1 |> Symbol.meaning)
                                ++ "."

                        head :: tail ->
                            (tail |> List.map (Card.description isGerman >> (\t -> "_" ++ t ++ "_")) |> String.join ", ")
                                ++ " and "
                                ++ (head |> Card.description isGerman |> (\t -> "_" ++ t ++ "_"))
                                ++ " should be read together. They refer to "
                                ++ (s1 |> Symbol.meaning)
                                ++ "."
                    )
                        ++ (case secondary of
                                Just ( s2, l2 ) ->
                                    case l2 of
                                        [] ->
                                            ""

                                        [ head ] ->
                                            " Additionally, "
                                                ++ (head |> Card.description isGerman >> (\t -> "_" ++ t ++ "_"))
                                                ++ " refers to "
                                                ++ (s2 |> Symbol.meaning)
                                                ++ "."

                                        [ _, _, _ ] ->
                                            " Additionally, All three cards should be read together and refer to "
                                                ++ (s2 |> Symbol.meaning)
                                                ++ "."

                                        head :: tail ->
                                            " Additionally, "
                                                ++ (tail |> List.map (Card.description isGerman >> (\t -> "_" ++ t ++ "_")) |> String.join ", ")
                                                ++ " and "
                                                ++ (head |> Card.description isGerman |> (\t -> "_" ++ t ++ "_"))
                                                ++ " should be read together. They refer to "
                                                ++ (s2 |> Symbol.meaning)
                                                ++ "."

                                Nothing ->
                                    ""
                           )

                Nothing ->
                    let
                        triangle =
                            list
                                |> List.find (Interpretation.fromCard >> Tuple.first >> .symbol >> (==) Triangle)
                                |> Maybe.withDefault c1

                        circle =
                            list
                                |> List.find (Interpretation.fromCard >> Tuple.first >> .symbol >> (==) Circle)
                                |> Maybe.withDefault c1

                        square =
                            list
                                |> List.find (Interpretation.fromCard >> Tuple.first >> .symbol >> (==) Square)
                                |> Maybe.withDefault c1
                    in
                    "Each card represents a different aspect: "
                        ++ ((triangle |> Card.description isGerman |> (\t -> "_" ++ t ++ "_")) ++ " refers to " ++ (Triangle |> Symbol.meaning) ++ ", ")
                        ++ ((square |> Card.description isGerman |> (\t -> "_" ++ t ++ "_")) ++ " refers to " ++ (Square |> Symbol.meaning) ++ " and ")
                        ++ ((circle |> Card.description isGerman |> (\t -> "_" ++ t ++ "_")) ++ " refers to " ++ (Circle |> Symbol.meaning) ++ ".")
                        ++ (case secondary of
                                Just ( s2, l2 ) ->
                                    case l2 of
                                        [] ->
                                            ""

                                        [ head ] ->
                                            " Additionally, "
                                                ++ (head |> Card.description isGerman |> (\t -> "_" ++ t ++ "_"))
                                                ++ "refers to "
                                                ++ (s2 |> Symbol.meaning)
                                                ++ "."

                                        [ _, _, _ ] ->
                                            " Additionally, All three cards should be read together and refer to "
                                                ++ (s2 |> Symbol.meaning)
                                                ++ "."

                                        head :: tail ->
                                            " Additionally, "
                                                ++ (tail |> List.map (Card.description isGerman >> (\t -> "_" ++ t ++ "_")) |> String.join ", ")
                                                ++ " and "
                                                ++ (head |> Card.description isGerman |> (\t -> "_" ++ t ++ "_"))
                                                ++ " should be read together. They refer to "
                                                ++ (s2 |> Symbol.meaning)
                                                ++ "."

                                Nothing ->
                                    ""
                           )
           )
        ++ "</interactive>\n"


sectionAspects : Bool -> ( Card, Card, Card ) -> String
sectionAspects isGerman ( c1, c2, c3 ) =
    let
        list =
            [ c1, c2, c3 ]

        ( primary, secondary ) =
            list
                |> findSymbolism

        cardParagraphFirst c =
            ("#### " ++ (c |> Card.description isGerman) ++ "\n")
                ++ (c
                        |> Interpretation.fromCard
                        |> Tuple.first
                        |> .text
                   )

        cardParagraphSecond s c =
            ("#### " ++ (c |> Card.description isGerman) ++ "\n")
                ++ (c
                        |> Interpretation.fromCard
                        |> (\( t1, t2 ) ->
                                if t1.symbol == s then
                                    t1.text

                                else
                                    t2
                                        |> Maybe.map .text
                                        |> Maybe.withDefault ""
                           )
                   )
    in
    "## Aspects\n"
        ++ (case ( primary, secondary ) of
                ( Just ( s1, l1 ), Just ( s2, l2 ) ) ->
                    ("### " ++ (s1 |> Symbol.meaning |> String.toTitleCase) ++ "\n")
                        ++ (l1
                                |> List.map cardParagraphFirst
                                |> String.join "\n\n"
                           )
                        ++ "\n"
                        ++ ("### " ++ (s2 |> Symbol.meaning |> String.toTitleCase) ++ "\n")
                        ++ (l2
                                |> List.map (cardParagraphSecond s2)
                                |> String.join "\n\n"
                           )

                ( Just ( s, l1 ), Nothing ) ->
                    ("### " ++ (s |> Symbol.meaning |> String.toTitleCase) ++ "\n")
                        ++ (l1
                                |> List.map cardParagraphFirst
                                |> String.join "\n\n"
                           )

                ( Nothing, Just ( s, l1 ) ) ->
                    let
                        triangle =
                            list
                                |> List.find (Interpretation.fromCard >> Tuple.first >> .symbol >> (==) Triangle)
                                |> Maybe.withDefault c1

                        circle =
                            list
                                |> List.find (Interpretation.fromCard >> Tuple.first >> .symbol >> (==) Circle)
                                |> Maybe.withDefault c1

                        square =
                            list
                                |> List.find (Interpretation.fromCard >> Tuple.first >> .symbol >> (==) Square)
                                |> Maybe.withDefault c1
                    in
                    ("### " ++ (Triangle |> Symbol.meaning |> String.toTitleCase) ++ "\n")
                        ++ (if s == Triangle then
                                l1
                                    |> List.map (cardParagraphSecond Triangle)
                                    |> String.join "\n\n"

                            else
                                cardParagraphSecond Triangle triangle
                                    ++ "\n\n"
                           )
                        ++ "\n"
                        ++ ("### " ++ (Square |> Symbol.meaning |> String.toTitleCase) ++ "\n")
                        ++ (if s == Square then
                                l1
                                    |> List.map (cardParagraphSecond Square)
                                    |> String.join "\n\n"

                            else
                                cardParagraphSecond Square square
                                    ++ "\n\n"
                           )
                        ++ "\n"
                        ++ ("### " ++ (Circle |> Symbol.meaning |> String.toTitleCase) ++ "\n")
                        ++ (if s == Circle then
                                l1
                                    |> List.map (cardParagraphSecond Circle)
                                    |> String.join "\n\n"

                            else
                                cardParagraphSecond Circle circle
                                    ++ "\n\n"
                           )

                ( Nothing, Nothing ) ->
                    let
                        triangle =
                            list
                                |> List.find (Interpretation.fromCard >> Tuple.first >> .symbol >> (==) Triangle)
                                |> Maybe.withDefault c1

                        circle =
                            list
                                |> List.find (Interpretation.fromCard >> Tuple.first >> .symbol >> (==) Circle)
                                |> Maybe.withDefault c1

                        square =
                            list
                                |> List.find (Interpretation.fromCard >> Tuple.first >> .symbol >> (==) Square)
                                |> Maybe.withDefault c1
                    in
                    ("### " ++ (Triangle |> Symbol.meaning |> String.toTitleCase) ++ "\n")
                        ++ (cardParagraphFirst triangle
                                ++ "\n\n"
                           )
                        ++ "\n"
                        ++ ("### " ++ (Square |> Symbol.meaning |> String.toSentenceCase) ++ "\n")
                        ++ (cardParagraphFirst square
                                ++ "\n\n"
                           )
                        ++ "\n"
                        ++ ("### " ++ (Circle |> Symbol.meaning |> String.toSentenceCase) ++ "\n")
                        ++ (cardParagraphFirst circle
                                ++ "\n\n"
                           )
           )


sectionNumerology : Bool -> ( Card, Card, Card ) -> String
sectionNumerology isGerman ( c1, c2, c3 ) =
    let
        list =
            [ c1, c2, c3 ]

        ( primary, secondary ) =
            list
                |> findSymbolism
    in
    "## Numerology"
