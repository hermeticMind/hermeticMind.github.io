module View.Color exposing (black, blackBackground, blue, dark, gray, green, light, primary, red, yellow)


blackBackground : String
blackBackground =
    black


black : String
black =
    if brandedOnly then
        light

    else
        "black"


light : String
light =
    "#ffffff"


dark : String
dark =
    "#3A3451"


primary : String
primary =
    "#D4E09B"


brandedOnly : Bool
brandedOnly =
    False


branded : String
branded =
    primary



--"LightGray"


gray : String
gray =
    "#666666"


green : String
green =
    if brandedOnly then
        branded

    else
        "#12BE52"



--"#00F600"


blue : String
blue =
    if brandedOnly then
        branded

    else
        "#1E88E5"



--"#3D04F2"


red : String
red =
    if brandedOnly then
        branded

    else
        "#DB4646"


yellow : String
yellow =
    if brandedOnly then
        branded

    else
        "#D8D815"
