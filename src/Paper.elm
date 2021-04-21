module Paper exposing (main)

import Browser
import Css
import Data.Alphabet as Alphabet
import Direction2d
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html as UnstyledHtml
import Html.Styled as Html exposing (Html)
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
import View.Page as Page


type alias Model =
    String


type Msg
    = GotResponse (Result Error String)


file =
    "constructionOfSigils"



--"logbook"


init : () -> ( Model, Cmd Msg )
init () =
    ( "loading...", getMarkdown GotResponse )


getMarkdown : (Result Error String -> Msg) -> Cmd Msg
getMarkdown gotResponse =
    Http.get
        { url =
            "https://raw.githubusercontent.com/HermeticMind/"
                ++ "HermeticMind.github.io/master/src/Markdown/"
                ++ file
                ++ ".md"
        , expect = Http.expectString gotResponse
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotResponse result ->
            case result of
                Ok string ->
                    ( string, Cmd.none )

                Err err ->
                    let
                        _ =
                            err
                                |> Debug.log "Error"
                    in
                    ( model, Cmd.none )


interactive : String -> Maybe String -> List (Element msg) -> Element msg
interactive name maybeValue content =
    (case name of
        "greekMagicSymbols" ->
            Alphabet.asList
                |> List.filterMap
                    (GreekMagicSymbol.fromChar 16
                        >> Maybe.map Element.html
                    )
                |> Element.row
                    [ Element.centerX
                    , Element.paddingEach
                        { top = 14
                        , right = 0
                        , bottom = 0
                        , left = 0
                        }
                    , Element.spacing 1
                    ]
                |> List.singleton

        "BinarySigil" ->
            let
                width : Float
                width =
                    40

                height : Float
                height =
                    55

                --64
            in
            List.range 0 (2 ^ 2 - 1)
                |> List.map
                    (\value ->
                        BinarySigil.view
                            { point =
                                Point2d.unsafe
                                    { x = width / 2, y = height / 2 }
                            , value = value
                            , size = 2
                            , color = "black"
                            , radius = 4
                            , strokeWidth = 2
                            , direction = Direction2d.positiveX
                            }
                            |> Svg.svg
                                [ SvgAttributes.width <| (String.fromFloat <| width) ++ "px"
                                , SvgAttributes.height <| (String.fromFloat <| height) ++ "px"
                                , SvgAttributes.version <| "1.1"
                                , SvgAttributes.viewBox <|
                                    "0 0 "
                                        ++ String.fromFloat width
                                        ++ " "
                                        ++ String.fromFloat height
                                ]
                            |> Element.html
                    )
                |> Element.row
                    [ Element.centerX
                    ]
                |> List.singleton

        "MagicSquareSigil" ->
            maybeValue
                |> Maybe.withDefault "Hermetic Mind"
                |> MagicSquareSigil.view
                    { size = 128
                    , zoom = 1
                    , strokeWidth = 2
                    , withBorder = False
                    , withText = True
                    , alphabet = Alphabet.english
                    }
                |> Element.html
                |> Element.el
                    [ Element.centerX
                    , Element.paddingEach
                        { top = 8
                        , right = 0
                        , bottom = 0
                        , left = 0
                        }
                    ]
                |> List.singleton

        "BraidSigil" ->
            maybeValue
                |> Maybe.withDefault "Hermetic Mind"
                |> BraidSigil.view
                    { width = 256
                    , height = 256
                    , radius = 100
                    , zoom = 1
                    , asAlphabet = Alphabet.english
                    , withCircle = True
                    , debugMode = False
                    , withRunes = False
                    , withText = False
                    , withBorder = False
                    , fillColor = "white"
                    , strokeColor = "black"
                    }
                |> Element.html
                |> Element.el
                    [ Element.centerX
                    , Element.paddingEach
                        { top = 8
                        , right = 0
                        , bottom = 0
                        , left = 0
                        }
                    ]
                |> List.singleton

        _ ->
            Element.text "Content not found"
                |> List.singleton
    )
        |> (\list ->
                list
                    ++ (content
                            |> Element.column
                                [ Element.centerX
                                ]
                            |> List.singleton
                       )
           )
        |> Element.column
            [ Border.width 1
            , Border.dashed
            , Border.color <| Element.rgb255 0 0 0
            , Element.padding 8
            , Border.rounded 8
            , Element.width <| Element.fill
            ]
        |> Element.el [ Element.paddingXY 32 16, Element.width <| Element.fill ]


view : Model -> UnstyledHtml.Html Msg
view model =
    (case model |> Parser.parse of
        Ok list ->
            case
                list
                    |> Renderer.render
                        (MarkdownRender.renderer interactive)
            of
                Ok elements ->
                    elements

                Err string ->
                    string |> Element.text |> List.singleton

        Err list ->
            list
                |> List.map (Parser.deadEndToString >> Element.text)
    )
        |> Element.column
            [ Element.width <| Element.px 700
            , Background.color <| Element.rgb255 255 255 255
            , Element.padding 64
            ]
        |> Page.view []


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
