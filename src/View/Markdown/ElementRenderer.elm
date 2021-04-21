module View.Markdown.ElementRenderer exposing (..)

import Css
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html)
import Html.Attributes as Attributes
import Markdown.Block as Block exposing (Block, HeadingLevel(..), Inline, ListItem(..), Task(..))
import Markdown.Html as Html
import Markdown.Renderer exposing (Renderer)


renderer : (String -> Maybe String -> List (Element msg) -> Element msg) -> Renderer (Element msg)
renderer interactive =
    let
        rawTextToId rawText =
            rawText
                |> String.split " "
                |> String.join "-"
                |> String.toLower

        tableBorder =
            [ Border.color (Element.rgb255 223 226 229)
            , Border.width 1
            , Border.solid
            , Element.paddingXY 6 13
            , Element.height Element.fill
            ]
    in
    { heading =
        \{ level, rawText, children } ->
            Element.paragraph
                ([ Font.size
                    (case level of
                        Block.H1 ->
                            36

                        Block.H2 ->
                            24

                        Block.H3 ->
                            18

                        _ ->
                            16
                    )
                 , Element.paddingEach
                    { top =
                        case level of
                            Block.H1 ->
                                0

                            Block.H2 ->
                                16

                            Block.H3 ->
                                18

                            _ ->
                                18
                    , right = 0
                    , bottom =
                        case level of
                            Block.H1 ->
                                32

                            _ ->
                                8
                    , left = 0
                    }
                 , Region.heading (Block.headingLevelToInt level)
                 , Element.htmlAttribute
                    (Attributes.attribute "name" (rawTextToId rawText))
                 , Element.htmlAttribute
                    (Attributes.id (rawTextToId rawText))
                 ]
                    ++ (case level of
                            Block.H1 ->
                                Font.family [ Font.typeface "Dancing Script" ]
                                    |> List.singleton

                            Block.H2 ->
                                Font.family [ Font.typeface "Dancing Script" ]
                                    |> List.singleton

                            _ ->
                                []
                       )
                )
                children

    -- [Font.family [ Font.typeface "Dancing Script" ]]
    , paragraph =
        Element.paragraph
            []

    --[ Element.spacing 15 ]
    , hardLineBreak = Html.br [] [] |> Element.html
    , blockQuote =
        \children ->
            Element.paragraph
                [ Border.widthEach { top = 0, right = 0, bottom = 0, left = 10 }
                , Element.padding 10
                , Border.color (Element.rgb255 145 145 145)
                , Background.color (Element.rgb255 245 245 245)
                ]
                children
    , strong =
        \content -> Element.paragraph [ Font.bold ] content
    , emphasis =
        \content -> Element.paragraph [ Font.italic ] content
    , codeSpan =
        \snippet ->
            Element.el
                [ Background.color
                    (Element.rgba 0 0 0 0.04)
                , Border.rounded 2
                , Element.paddingXY 5 3
                , Font.family
                    [ Font.external
                        { url = "https://fonts.googleapis.com/css?family=Source+Code+Pro"
                        , name = "Source Code Pro"
                        }
                    ]
                ]
                (Element.text snippet)
    , link =
        \{ title, destination } body ->
            Element.newTabLink []
                { url = destination
                , label =
                    Element.paragraph
                        [ Font.color (Element.rgb255 0 0 255)
                        , Element.htmlAttribute (Attributes.style "overflow-wrap" "break-word")
                        , Element.htmlAttribute (Attributes.style "word-break" "break-word")
                        ]
                        body
                }
    , image =
        \image ->
            case image.title of
                Just title ->
                    Element.image [ Element.width Element.fill ] { src = image.src, description = image.alt }

                Nothing ->
                    Element.image [ Element.width Element.fill ] { src = image.src, description = image.alt }
    , text =
        \value -> Element.paragraph [] [ Element.text value ]
    , unorderedList =
        \items ->
            Element.column [ Element.spacing 15 ]
                (items
                    |> List.map
                        (\(ListItem task children) ->
                            Element.paragraph [ Element.spacing 5 ]
                                [ Element.paragraph
                                    [ Element.alignTop ]
                                    ((case task of
                                        IncompleteTask ->
                                            Input.defaultCheckbox False

                                        CompletedTask ->
                                            Input.defaultCheckbox True

                                        NoTask ->
                                            Element.text "•"
                                     )
                                        :: Element.text " "
                                        :: children
                                    )
                                ]
                        )
                )
    , orderedList =
        \startingIndex items ->
            Element.column [ Element.spacing 15 ]
                (items
                    |> List.indexedMap
                        (\index itemBlocks ->
                            Element.paragraph [ Element.spacing 5 ]
                                [ Element.paragraph [ Element.alignTop ]
                                    (Element.text (String.fromInt (index + startingIndex) ++ " ") :: itemBlocks)
                                ]
                        )
                )
    , html =
        Html.oneOf
            [ Html.tag "page"
                (Element.column
                    [ Element.padding <| 50
                    ]
                )
            , Html.tag "interactive" interactive
                |> Html.withAttribute "name"
                |> Html.withOptionalAttribute "value"
            , Html.tag "abstract" (Element.column [])
            , Html.tag "image"
                (\title height src _ ->
                    [ Element.image
                        (height
                            |> Maybe.map
                                (String.toInt
                                    >> Maybe.withDefault 0
                                    >> Element.px
                                    >> Element.height
                                    >> List.singleton
                                )
                            |> Maybe.withDefault []
                        )
                        { src = src
                        , description = title
                        }
                        |> Element.el
                            [ Element.centerX
                            ]
                    , title
                        |> Element.text
                        |> Element.el
                            [ Element.centerX
                            , Font.italic
                            ]
                    ]
                        |> Element.column
                            [ Element.width <| Element.fill, Element.paddingXY 0 8 ]
                )
                |> Html.withAttribute "title"
                |> Html.withOptionalAttribute "height"
                |> Html.withAttribute "src"
            ]
    , codeBlock =
        \details ->
            Element.paragraph
                [ Background.color (Element.rgba 0 0 0 0.03)
                , Element.htmlAttribute (Attributes.style "white-space" "pre")
                , Element.htmlAttribute (Attributes.style "overflow-wrap" "break-word")
                , Element.htmlAttribute (Attributes.style "word-break" "break-word")
                , Element.padding 20
                , Font.family
                    [ Font.external
                        { url = "https://fonts.googleapis.com/css?family=Source+Code+Pro"
                        , name = "Source Code Pro"
                        }
                    ]
                ]
                [ Element.text details.body ]
    , thematicBreak = Element.none
    , table = Element.column []
    , tableHeader =
        Element.column
            [ Font.bold
            , Element.width Element.fill
            , Font.center
            ]
    , tableBody = Element.column []
    , tableRow = Element.row [ Element.height Element.fill, Element.width Element.fill ]
    , tableHeaderCell =
        \maybeAlignment children ->
            Element.paragraph
                tableBorder
                children
    , tableCell =
        \maybeAlignment children ->
            Element.paragraph
                tableBorder
                children
    , strikethrough = \content -> Element.paragraph [ Font.strike ] content
    }
