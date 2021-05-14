module Data.Oracle exposing (reading)

import Array
import Binary
import Data.Attribute as Attribute exposing (Attribute)
import Data.Card as Card exposing (Card(..))
import Data.Card.Interpretation as Interpretation
import Data.Symbol as Symbol exposing (Symbol(..))
import List.Extra as List
import String.Extra as String


listToSentence : Bool -> List String -> String
listToSentence isGerman list =
    case list |> List.reverse of
        [] ->
            ""

        [ a ] ->
            a

        head :: tail ->
            (tail
                |> List.reverse
                |> String.join ", "
            )
                ++ (if isGerman then
                        " und "

                    else
                        " and "
                   )
                ++ head


reading : Bool -> String -> ( Card, Card, Card ) -> String
reading isGerman string cards =
    "<page>"
        ++ ("# "
                ++ (if isGerman then
                        "Auswertung der Lesung"

                    else
                        "Spread Report"
                   )
                ++ "\n "
           )
        ++ (if isGerman then
                "Für diese Lesung benützen wir drei Karten."
                    ++ "Die visuelle, semantische sowie numerologische Ähnlichkeiten zwischen den Karten werden anschließend für die Auswertung genützt."

            else
                "For this reading we are using three cards. "
                    ++ "The visual, semantical and numerological similarities between the cards are used for the interpretation.\n"
           )
        ++ sectionYourCards isGerman cards
        ++ (if string == "" then
                ""

            else
                (if isGerman then
                    "## Zusammenfassung\n"

                 else
                    "## Summary\n"
                )
                    ++ (string ++ "\n")
           )
        ++ sectionAspects isGerman cards
        ++ "</page><page>"
        ++ sectionNumerology isGerman cards
        ++ "</page>"


findSymbolism : Bool -> List Card -> ( Maybe ( Symbol, List Card ), Maybe ( Symbol, List Card ) )
findSymbolism isGerman =
    List.map (\c -> ( c, Interpretation.fromCard isGerman c ))
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
                |> findSymbolism isGerman
    in
    "<box>"
        ++ ("<interactive name=\"Oracle\" value=\"" ++ (c1 |> Card.encode) ++ "\"></interactive>\n")
        ++ ("<interactive name=\"Oracle\" value=\"" ++ (c2 |> Card.encode) ++ "\"></interactive>\n")
        ++ ("<interactive name=\"Oracle\" value=\"" ++ (c3 |> Card.encode) ++ "\"></interactive>\n")
        ++ "</box>"
        ++ (case primary of
                Just ( s1, l1 ) ->
                    (case l1 of
                        [ _, _, _ ] ->
                            (if isGerman then
                                "Die drei Karten sollten zusammen gelesen werden und beziehen sich auf "

                             else
                                "All three cards should be read together and refer to "
                            )
                                ++ (s1 |> Symbol.meaning isGerman)
                                ++ "."

                        l ->
                            (l
                                |> List.map (Card.description isGerman >> (\t -> "_" ++ t ++ "_"))
                                |> listToSentence isGerman
                            )
                                ++ (if isGerman then
                                        " sollten zusammen gelesen werden und beziehen sich auf "

                                    else
                                        " should be read together. They refer to "
                                   )
                                ++ (s1 |> Symbol.meaning isGerman)
                                ++ "."
                    )
                        ++ (case secondary of
                                Just ( s2, l2 ) ->
                                    case l2 of
                                        [] ->
                                            ""

                                        [ head ] ->
                                            (if isGerman then
                                                " Außerdem bezieht sich "

                                             else
                                                " Additionally, "
                                            )
                                                ++ (head |> Card.description isGerman >> (\t -> "_" ++ t ++ "_"))
                                                ++ (if isGerman then
                                                        " auf "

                                                    else
                                                        " refers to "
                                                   )
                                                ++ (s2 |> Symbol.meaning isGerman)
                                                ++ "."

                                        [ _, _, _ ] ->
                                            (if isGerman then
                                                " Außerdem beziehen sich alle drei Karten auf "

                                             else
                                                " Additionally, All three cards refer to "
                                            )
                                                ++ (s2 |> Symbol.meaning isGerman)
                                                ++ "."

                                        l ->
                                            (if isGerman then
                                                "Außerdem sollten "

                                             else
                                                " Additionally, "
                                            )
                                                ++ (l
                                                        |> List.map (Card.description isGerman >> (\t -> "_" ++ t ++ "_"))
                                                        |> listToSentence isGerman
                                                   )
                                                ++ (if isGerman then
                                                        " gemeinsam gelesen werden und beziehen sich auf "

                                                    else
                                                        " should be read together. They refer to "
                                                   )
                                                ++ (s2 |> Symbol.meaning isGerman)
                                                ++ "."

                                Nothing ->
                                    ""
                           )

                Nothing ->
                    let
                        triangle =
                            list
                                |> List.find (Interpretation.fromCard isGerman >> Tuple.first >> .symbol >> (==) Triangle)
                                |> Maybe.withDefault c1

                        circle =
                            list
                                |> List.find (Interpretation.fromCard isGerman >> Tuple.first >> .symbol >> (==) Circle)
                                |> Maybe.withDefault c1

                        square =
                            list
                                |> List.find (Interpretation.fromCard isGerman >> Tuple.first >> .symbol >> (==) Square)
                                |> Maybe.withDefault c1
                    in
                    (if isGerman then
                        "Jede Karte steht für einen anderen Aspekt: "

                     else
                        "Each card represents a different aspect: "
                    )
                        ++ ((triangle |> Card.description isGerman |> (\t -> "_" ++ t ++ "_"))
                                ++ (if isGerman then
                                        " bezieht sich auf "

                                    else
                                        " refers to "
                                   )
                                ++ (Triangle |> Symbol.meaning isGerman)
                                ++ ", "
                           )
                        ++ ((square |> Card.description isGerman |> (\t -> "_" ++ t ++ "_"))
                                ++ (if isGerman then
                                        " bezieht sich auf "

                                    else
                                        " refers to "
                                   )
                                ++ (Square |> Symbol.meaning isGerman)
                                ++ (if isGerman then
                                        " und "

                                    else
                                        " and "
                                   )
                           )
                        ++ ((circle |> Card.description isGerman |> (\t -> "_" ++ t ++ "_"))
                                ++ (if isGerman then
                                        " bezieht sich auf "

                                    else
                                        " refers to "
                                   )
                                ++ (Circle |> Symbol.meaning isGerman)
                                ++ "."
                           )
                        ++ (case secondary of
                                Just ( s2, l2 ) ->
                                    case l2 of
                                        [] ->
                                            ""

                                        [ head ] ->
                                            (if isGerman then
                                                "Außerdem bezieht sich "

                                             else
                                                " Additionally, "
                                            )
                                                ++ (head |> Card.description isGerman |> (\t -> "_" ++ t ++ "_"))
                                                ++ (if isGerman then
                                                        " auf "

                                                    else
                                                        " refers to "
                                                   )
                                                ++ (s2 |> Symbol.meaning isGerman)
                                                ++ "."

                                        [ _, _, _ ] ->
                                            (if isGerman then
                                                " Additionally, all three cards should be read together and refer to "

                                             else
                                                " Außerdem beziehen sich alle drei Karten auf "
                                            )
                                                ++ (s2 |> Symbol.meaning isGerman)
                                                ++ "."

                                        l ->
                                            (if isGerman then
                                                " Außerdem beziehen sich "

                                             else
                                                " Additionally, "
                                            )
                                                ++ (l
                                                        |> List.map (Card.description isGerman >> (\t -> "_" ++ t ++ "_"))
                                                        |> listToSentence isGerman
                                                   )
                                                ++ (if isGerman then
                                                        " auf "

                                                    else
                                                        " should be read together. They refer to "
                                                   )
                                                ++ (s2 |> Symbol.meaning isGerman)
                                                ++ "."

                                Nothing ->
                                    ""
                           )
           )
        ++ "\n"


sectionAspects : Bool -> ( Card, Card, Card ) -> String
sectionAspects isGerman ( c1, c2, c3 ) =
    let
        list =
            [ c1, c2, c3 ]

        ( primary, secondary ) =
            list
                |> findSymbolism isGerman

        cardParagraphFirst c =
            ("#### " ++ (c |> Card.description isGerman) ++ "\n")
                ++ (c
                        |> Interpretation.fromCard isGerman
                        |> Tuple.first
                        |> .text
                   )

        cardParagraphSecond s c =
            ("#### " ++ (c |> Card.description isGerman) ++ "\n")
                ++ (c
                        |> Interpretation.fromCard isGerman
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
    (if isGerman then
        "## Aspekte\n"

     else
        "## Aspects\n"
    )
        ++ (case ( primary, secondary ) of
                ( Just ( s1, l1 ), Just ( s2, l2 ) ) ->
                    ("### " ++ (s1 |> Symbol.meaning isGerman |> String.toTitleCase) ++ "\n")
                        ++ (l1
                                |> List.map cardParagraphFirst
                                |> String.join "\n\n"
                           )
                        ++ "\n"
                        ++ ("### " ++ (s2 |> Symbol.meaning isGerman |> String.toTitleCase) ++ "\n")
                        ++ (l2
                                |> List.map (cardParagraphSecond s2)
                                |> String.join "\n\n"
                           )

                ( Just ( s, l1 ), Nothing ) ->
                    ("### " ++ (s |> Symbol.meaning isGerman |> String.toTitleCase) ++ "\n")
                        ++ (l1
                                |> List.map cardParagraphFirst
                                |> String.join "\n\n"
                           )

                ( Nothing, Just ( s, l1 ) ) ->
                    let
                        triangle =
                            list
                                |> List.find (Interpretation.fromCard isGerman >> Tuple.first >> .symbol >> (==) Triangle)
                                |> Maybe.withDefault c1

                        circle =
                            list
                                |> List.find (Interpretation.fromCard isGerman >> Tuple.first >> .symbol >> (==) Circle)
                                |> Maybe.withDefault c1

                        square =
                            list
                                |> List.find (Interpretation.fromCard isGerman >> Tuple.first >> .symbol >> (==) Square)
                                |> Maybe.withDefault c1
                    in
                    ("### " ++ (Triangle |> Symbol.meaning isGerman |> String.toTitleCase) ++ "\n")
                        ++ (if s == Triangle then
                                l1
                                    |> List.map (cardParagraphSecond Triangle)
                                    |> String.join "\n\n"

                            else
                                cardParagraphSecond Triangle triangle
                                    ++ "\n\n"
                           )
                        ++ "\n"
                        ++ ("### " ++ (Square |> Symbol.meaning isGerman |> String.toTitleCase) ++ "\n")
                        ++ (if s == Square then
                                l1
                                    |> List.map (cardParagraphSecond Square)
                                    |> String.join "\n\n"

                            else
                                cardParagraphSecond Square square
                                    ++ "\n\n"
                           )
                        ++ "\n"
                        ++ ("### " ++ (Circle |> Symbol.meaning isGerman |> String.toTitleCase) ++ "\n")
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
                                |> List.find (Interpretation.fromCard isGerman >> Tuple.first >> .symbol >> (==) Triangle)
                                |> Maybe.withDefault c1

                        circle =
                            list
                                |> List.find (Interpretation.fromCard isGerman >> Tuple.first >> .symbol >> (==) Circle)
                                |> Maybe.withDefault c1

                        square =
                            list
                                |> List.find (Interpretation.fromCard isGerman >> Tuple.first >> .symbol >> (==) Square)
                                |> Maybe.withDefault c1
                    in
                    ("### " ++ (Triangle |> Symbol.meaning isGerman |> String.toTitleCase) ++ "\n")
                        ++ (cardParagraphFirst triangle
                                ++ "\n\n"
                           )
                        ++ "\n"
                        ++ ("### " ++ (Square |> Symbol.meaning isGerman |> String.toSentenceCase) ++ "\n")
                        ++ (cardParagraphFirst square
                                ++ "\n\n"
                           )
                        ++ "\n"
                        ++ ("### " ++ (Circle |> Symbol.meaning isGerman |> String.toSentenceCase) ++ "\n")
                        ++ (cardParagraphFirst circle
                                ++ "\n\n"
                           )
           )
        ++ "\n"


attributeDifference : ( Card, Card, Card ) -> List Int
attributeDifference ( c1, c2, c3 ) =
    let
        toBinary c =
            c
                |> Card.value
                |> (+) -1
                |> Binary.fromDecimal
                |> Binary.ensureSize
                    (case c of
                        Back ->
                            -1

                        Joker ->
                            0

                        Binary _ ->
                            1

                        Element _ ->
                            2

                        Planet _ ->
                            3

                        Virtue _ ->
                            4

                        Trump _ ->
                            5
                    )
                |> Binary.toIntegers

        a1 =
            toBinary c1 |> List.reverse |> Array.fromList

        a2 =
            toBinary c2 |> List.reverse |> Array.fromList

        a3 =
            toBinary c3 |> List.reverse |> Array.fromList

        combine i =
            [ a1 |> Array.get i
            , a2 |> Array.get i
            , a3 |> Array.get i
            ]
                |> List.filterMap identity
                |> List.partition ((==) 0)
                |> (\( posList, negList ) ->
                        if (posList |> List.length) == (negList |> List.length) then
                            2

                        else if (posList |> List.length) > (negList |> List.length) then
                            1

                        else
                            0
                   )
    in
    [ combine 0
    , combine 1
    , combine 2
    , combine 3
    ]


attributes : ( Card, Card, Card ) -> List ( Attribute, List Card )
attributes ( c1, c2, c3 ) =
    let
        toBinary c =
            c
                |> Card.value
                |> (+) -1
                |> Binary.fromDecimal
                |> Binary.ensureSize
                    (case c of
                        Back ->
                            -1

                        Joker ->
                            0

                        Binary _ ->
                            1

                        Element _ ->
                            2

                        Planet _ ->
                            3

                        Virtue _ ->
                            4

                        Trump _ ->
                            5
                    )
                |> Binary.toIntegers

        a1 =
            toBinary c1 |> List.reverse |> Array.fromList

        a2 =
            toBinary c2 |> List.reverse |> Array.fromList

        a3 =
            toBinary c3 |> List.reverse |> Array.fromList

        combine i =
            [ ( c1, a1 |> Array.get i )
            , ( c2, a2 |> Array.get i )
            , ( c3, a3 |> Array.get i )
            ]
                |> List.filterMap (\( c, m ) -> m |> Maybe.map (\n -> ( c, n )))
                |> List.partition (\( c, n ) -> n == 0)
                |> (\( posList, negList ) ->
                        if (posList |> List.length) == (negList |> List.length) then
                            Nothing

                        else if (posList |> List.length) > (negList |> List.length) then
                            Just ( Attribute.fromDegree (i + 1) True, posList |> List.map Tuple.first )

                        else
                            Just ( Attribute.fromDegree (i + 1) False, negList |> List.map Tuple.first )
                   )
    in
    [ combine 0
    , combine 1
    , combine 2
    , combine 3
    ]
        |> List.filterMap identity


sectionNumerology : Bool -> ( Card, Card, Card ) -> String
sectionNumerology isGerman ( c1, c2, c3 ) =
    let
        list =
            [ c1, c2, c3 ]

        toBinary c =
            c
                |> Card.value
                |> (+) -1
                |> Binary.fromDecimal
                |> Binary.ensureSize
                    (case c of
                        Back ->
                            -1

                        Joker ->
                            0

                        Binary _ ->
                            1

                        Element _ ->
                            2

                        Planet _ ->
                            3

                        Virtue _ ->
                            4

                        Trump _ ->
                            5
                    )
                |> Binary.toIntegers

        b1 =
            toBinary c1

        b2 =
            toBinary c2

        b3 =
            toBinary c3

        attrList =
            attributes ( c1, c2, c3 )
    in
    (if isGerman then
        "## Numerologie\n"

     else
        "## Numerology\n"
    )
        ++ (if isGerman then
                "Jeder Karte kann eine eindeutige Sigille zugeordnet werden. "
                    ++ "Diese Sigillen sind visuelle representationen von numerologische Konzepte. "

            else
                "Each card can be represented as a unique sigil. "
                    ++ "This sigils are visual representations of numerological concepts."
           )
        ++ ("<box>"
                ++ ([ b1, b2, b3 ]
                        |> List.map
                            (List.map String.fromInt
                                >> String.concat
                                >> (\value ->
                                        "<interactive name=\"BinarySigil\" value=\"" ++ value ++ "\"></interactive>\n"
                                   )
                            )
                        |> String.concat
                   )
                ++ "</box>\n"
           )
        ++ ((if isGerman then
                "Das erste Symbol steht für "

             else
                "The first symbol stands for "
            )
                ++ (c1
                        |> Card.title isGerman
                        |> (if isGerman then
                                identity

                            else
                                String.toLower
                           )
                   )
                ++ ",  "
           )
        ++ ((if isGerman then
                "das zweite Symbol steht für "

             else
                "the second symbol stands for "
            )
                ++ (c2
                        |> Card.title isGerman
                        |> (if isGerman then
                                identity

                            else
                                String.toLower
                           )
                   )
                ++ " and "
           )
        ++ ((if isGerman then
                "das dritte Symbol steht für "

             else
                "the third symbol stands for "
            )
                ++ (c3
                        |> Card.title isGerman
                        |> (if isGerman then
                                identity

                            else
                                String.toLower
                           )
                   )
                ++ ".\n"
           )
        ++ (attrList
                |> List.map
                    (\( attr, l ) ->
                        ("### " ++ (attr |> Attribute.toString isGerman) ++ "\n")
                            ++ (l
                                    |> List.map (Card.title isGerman)
                                    |> listToSentence isGerman
                               )
                            ++ (if (l |> List.length) == 1 then
                                    if isGerman then
                                        " steht"

                                    else
                                        " stands"

                                else if isGerman then
                                    " stehen"

                                else
                                    " stand"
                               )
                            ++ (if isGerman then
                                    " für "

                                else
                                    " for being "
                               )
                            ++ "_"
                            ++ (attr |> Attribute.toString isGerman)
                            ++ "_.\n"
                            ++ (attr |> Attribute.description isGerman)
                            ++ "\n"
                    )
                |> String.concat
           )
        ++ ("<box><interactive name=\"Attributes\" value=\""
                ++ (( c1, c2, c3 )
                        |> attributeDifference
                        |> List.map String.fromInt
                        |> String.concat
                   )
                ++ "\"></interactive></box>"
           )
