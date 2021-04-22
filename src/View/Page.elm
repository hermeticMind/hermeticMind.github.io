module View.Page exposing (..)

import Browser exposing (Document)
import Css
import Data.Alphabet as Alphabet
import Direction2d
import Element exposing (Attribute, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Http exposing (Error)
import Markdown.Block exposing (HeadingLevel)
import Markdown.Parser as Parser
import Markdown.Renderer as Renderer exposing (Renderer)
import Point2d
import Quantity
import Svg
import Svg.Attributes as SvgAttributes
import View.BinarySigil as BinarySigil
import View.BraidSigil as BraidSigil
import View.GreekMagicSymbol as GreekMagicSymbol
import View.MagicSquareSigil as MagicSquareSigil
import View.Markdown.ElementRenderer as MarkdownRender


view : List (Attribute msg) -> Element msg -> Document msg
view attr content =
    { title = "Hermetic Mind"
    , body =
        [ Element.none
            |> Element.el [ Element.width <| Element.fill ]
        , content
        , Element.none
            |> Element.el [ Element.width <| Element.fill ]
        ]
            |> Element.row
                [ Element.width <| Element.fill
                , Element.padding 64
                , Element.height <| Element.fill
                ]
            |> Element.layoutWith
                { options =
                    [ Element.focusStyle
                        { borderColor = Nothing
                        , backgroundColor = Nothing
                        , shadow = Nothing
                        }
                    ]
                }
                ([ Background.color <| Element.rgb255 48 0 48
                 , Font.family [ Font.typeface "Serif" ]
                 , Font.color <| Element.rgb255 32 32 32
                 , Font.size 14
                 , [ [ "Hermetic"
                        |> Element.text
                     , "Mind"
                        |> Element.text
                     ]
                        |> Element.column
                            [ Font.size 36
                            , Border.widthEach
                                { bottom = 1
                                , left = 0
                                , right = 0
                                , top = 0
                                }
                            , Border.color <| Element.rgb255 255 255 255
                            , Element.paddingEach
                                { top = 0
                                , right = 0
                                , bottom = 4
                                , left = 0
                                }
                            , Element.alignTop
                            , Element.alignLeft
                            ]
                   , Element.link
                        [ Font.shadow
                            { offset = ( 0, 0 )
                            , blur = 32
                            , color = Element.rgb255 0 0 255
                            }
                        ]
                        { url = "/oracle"
                        , label =
                            "Oracle (German)"
                                |> Element.text
                                |> Element.el []
                        }
                   , Element.link
                        [ Font.shadow
                            { offset = ( 0, 0 )
                            , blur = 32
                            , color = Element.rgb255 0 0 255
                            }
                        ]
                        { url = "/papers"
                        , label = "Technical Paper" |> Element.text
                        }
                   ]
                    |> Element.column
                        [ Element.alignRight
                        , Element.alignTop
                        , Font.size 16
                        , Font.color <| Element.rgb255 255 255 255
                        , Element.spacing 8
                        , Element.width <| Element.px 150
                        ]
                    |> Element.el
                        [ Element.paddingEach
                            { top = 64
                            , right = 0
                            , bottom = 0
                            , left = 0
                            }
                        , Element.centerX
                        , Element.moveLeft 450
                        ]
                    |> Element.inFront
                 ]
                    ++ attr
                )
            |> List.singleton
    }
