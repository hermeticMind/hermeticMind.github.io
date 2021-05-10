module SigilGenerator exposing (..)

import Browser
import Data.Alphabet as Alphabet
import Data.Card exposing (description)
import Element exposing (Element)
import Element.Background as Background
import Element.Font as Font
import Html exposing (Html)
import List.Extra as List
import View.BraidSigil as BraidSigil
import View.MagicSquareSigil as MagicSquareSigil
import Widget
import Widget.Customize as Customize
import Widget.Material as Material
import Widget.Material.Typography as Typography



--------------------------------------------------------------------------------
-- Config
--------------------------------------------------------------------------------


type SigilSort
    = BraidSigil
    | MagicSquareSigil


sigilSort : SigilSort
sigilSort =
    BraidSigil


zoom : number
zoom =
    2



--4


radius : number
radius =
    60


size : number
size =
    radius * 3


width : number
width =
    size


height : Float
height =
    size * 1.25


withCircle : Bool
withCircle =
    True


interactive : Bool
interactive =
    True


debugMode : Bool
debugMode =
    False


withRunes : Bool
withRunes =
    False


withText : Bool
withText =
    True


withBorder : Bool
withBorder =
    False


paths =
    --[ "Glueck", "Liebe", "Geld", "Erfolg" ]
    --[ "Fortune", "Love", "Money", "Success", "Health", "Protection", "Energy", "Divination" ]
    [ "Gegenwart", "Ziel", "Kraft", "Bedeutung", "Dankbarkeit", "Werte", "Gedanken", "Zukunft" ]



{--[ "teat"
    , "aaaasssd"
    , "bede"
    , "uede"
    , "ebebebebe"
    , "aeiaei"
    , "aeiou"
    ]--}
{--[ "Gegenwart"
        , "Ziel"
        , "Werte"
        , "Kraft"
        , "Bedeutung"
        , "Dankbarkeit"
        , "Gedanken"
        , "Zukunft"
        ]--}
--------------------------------------------------------------------------------
-- Model
--------------------------------------------------------------------------------


type alias Model =
    { string : String
    , isGerman : Bool
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { string = ""
      , isGerman = False
      }
    , Cmd.none
    )



--------------------------------------------------------------------------------
-- Update
--------------------------------------------------------------------------------


type Msg
    = ChangedText String
    | SetField (Model -> Model)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangedText string ->
            ( { model | string = string }, Cmd.none )

        SetField fun ->
            ( fun model, Cmd.none )



--------------------------------------------------------------------------------
-- Subscriptions
--------------------------------------------------------------------------------


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------


switch :
    { description : String
    , onPress : Model -> Model
    , active : Bool
    }
    -> Element Msg
switch { description, onPress, active } =
    [ Element.text description
    , Widget.switch (Material.switch Material.defaultPalette)
        { description = description
        , onPress =
            onPress
                |> SetField
                |> Just
        , active = active
        }
    ]
        |> Element.row [ Element.spacing 4 ]


view : Model -> Html Msg
view model =
    let
        viewFun =
            case sigilSort of
                BraidSigil ->
                    BraidSigil.view
                        { width = width
                        , height = height
                        , radius = radius
                        , zoom = zoom
                        , asAlphabet =
                            if model.isGerman then
                                Alphabet.german

                            else
                                Alphabet.english
                        , withCircle = withCircle
                        , debugMode = debugMode
                        , withRunes = withRunes
                        , withText = withText
                        , withBorder = withBorder
                        , fillColor = "white"
                        , strokeColor = "black"
                        }

                MagicSquareSigil ->
                    MagicSquareSigil.view
                        { size = width * 1.5
                        , zoom = 1
                        , strokeWidth = 5
                        , alphabet =
                            if model.isGerman then
                                Alphabet.german

                            else
                                Alphabet.english
                        , withText = withText
                        , withBorder = withBorder
                        }
    in
    if interactive then
        [ "Sigil Generator"
            |> Element.text
            |> Element.el Typography.h6
            |> Widget.asItem
        , Widget.fullBleedItem (Material.fullBleedItem Material.defaultPalette)
            { text = "Write a single word into the input field."
            , onPress = Nothing
            , icon = always Element.none
            }
        , Widget.textInput (Material.textInput Material.defaultPalette)
            { chips = []
            , text = model.string
            , placeholder = Nothing
            , label = "Word"
            , onChange = ChangedText
            }
            |> Widget.asItem
        , Widget.divider (Material.fullBleedDivider Material.defaultPalette)
        , [ switch
                { description = "is German"
                , onPress = \m -> { m | isGerman = not model.isGerman }
                , active = model.isGerman
                }
          ]
            |> Element.wrappedRow [ Element.width <| Element.fill ]
            |> Widget.asItem
        , Widget.divider (Material.fullBleedDivider Material.defaultPalette)
        , viewFun
            model.string
            |> Element.html
            |> Element.el [ Element.centerX ]
            |> Widget.asItem
        , Widget.divider (Material.fullBleedDivider Material.defaultPalette)
        , [ Element.text "This work is licensed under a "
          , Element.link [ Font.underline, Font.color <| Element.rgb255 100 80 146 ]
                { url = "https://creativecommons.org/licenses/by-nc/4.0/"
                , label = Element.text "Creative Commons Attribution-NonCommercial 4.0 International License"
                }
          , Element.text " by "
          , Element.link [ Font.underline, Font.color <| Element.rgb255 100 80 146 ]
                { url = "https://www.etsy.com/shop/HermeticMind"
                , label = Element.text "DI Lucas Payr"
                }
          , Element.text "."
          ]
            |> Element.paragraph []
            |> Widget.asItem
        ]
            |> Widget.itemList
                (Material.cardColumn Material.defaultPalette
                    |> Customize.elementColumn
                        [ Element.width <| Element.px <| round <| zoom * width * 1.5
                        , Element.centerX
                        , Element.centerY
                        ]
                    |> Customize.mapContent
                        (Customize.element
                            [ Element.width <| Element.px <| round <| zoom * width * 1.5
                            ]
                        )
                )
            |> Element.layout [ Background.color <| Element.rgb255 64 64 64 ]

    else
        paths
            |> List.map viewFun
            |> Html.div []


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
