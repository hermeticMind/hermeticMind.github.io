module OracleReading exposing (..)

import Browser
import Css
import Data.Card as Card exposing (Card(..))
import Data.Oracle as Oracle
import Html as UnstyledHtml
import Html.Styled as Html
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Markdown.Parser as Parser
import Markdown.Renderer as Renderer
import Random
import Random.List as Random
import View.Interactive as Interactive
import View.Markdown.HtmlRenderer as MarkdownRender


type alias Model =
    { cards : ( Card, Card, Card )
    , summary : String
    }


type Msg
    = GotCards (List Card)
    | ChangeSummary String


file =
    "constructionOfSigils"



--"logbook"


init : () -> ( Model, Cmd Msg )
init () =
    let
        cards : Maybe ( Card, Card, Card )
        cards =
            Just ( Trump 15, Element 2, Trump 8 )
    in
    case cards of
        Just c ->
            ( { cards = c
              , summary = ""
              }
            , Cmd.none
            )

        Nothing ->
            ( { cards = ( Back, Back, Back )
              , summary = ""
              }
            , Random.generate GotCards
                (Random.choices 3 Card.asList
                    |> Random.map Tuple.first
                )
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotCards cards ->
            case cards of
                [ c1, c2, c3 ] ->
                    ( { model | cards = ( c1, c2, c3 ) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ChangeSummary string ->
            ( { model | summary = string }, Cmd.none )


view : Model -> UnstyledHtml.Html Msg
view model =
    let
        isGerman : Bool
        isGerman =
            False
    in
    case model.cards |> Oracle.reading isGerman model.summary |> Parser.parse of
        Ok list ->
            case
                list
                    |> Renderer.render
                        (MarkdownRender.renderer (Interactive.view isGerman))
            of
                Ok elements ->
                    Html.input
                        [ Events.onInput <| ChangeSummary
                        , Attributes.value <| model.summary
                        ]
                        []
                        :: elements
                        |> Html.div
                            [ Attributes.css
                                [ Css.width <| Css.px 800
                                , Css.margin2 (Css.px 0) Css.auto
                                , Css.backgroundColor (Css.rgb 255 255 255)
                                ]
                            ]
                        |> List.singleton
                        |> Html.div
                            [ Attributes.css
                                [ Css.fontSize (Css.px 14)
                                , Css.backgroundColor (Css.rgb 64 64 64)
                                ]
                            ]
                        |> Html.toUnstyled

                Err string ->
                    string |> UnstyledHtml.text

        Err list ->
            list
                |> List.map (Parser.deadEndToString >> UnstyledHtml.text)
                |> UnstyledHtml.div []


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
