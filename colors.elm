import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Ev
import Color exposing (..)
import Color.Convert exposing (..)
import Kintail.InputWidget as InputWidget
import Html.Events exposing (onClick)
import Round
type alias Model = Color


model : Model
model = rgb 0 0 0
type Title
  = NamedColors
  | Mint
  | Orange
  | Lavander
  | Pink
  | Cornflower
  | Gold
  | Fuchsia
  | Plum
  | Orchard
  | Teal
colors = [NamedColors, Mint, Orange, Lavander, Pink, Cornflower, Gold, Fuchsia, Plum, Orchard, Teal]
type Msg
  = ChangeRed Int
  | ChangeGreen Int
  | ChangeBlue Int
  | ChangeHue Float     {---degrees---}
  | ChangeSat Float     {-0.00 : 1.00-}
  | ChangeLight Float   {-0.00 : 1.00-}
  | NewTitle Title
  | Complement

update : Msg -> Model -> Model
update msg model =
  let c = toRgb model 
      hsv = toHsl model
  in case msg of
       ChangeRed r -> rgb r c.green c.blue
       ChangeGreen g -> rgb c.red g c.blue
       ChangeBlue b -> rgb c.red c.green b
       ChangeHue h -> hsl h hsv.saturation hsv.lightness
       ChangeSat s -> hsl hsv.hue s hsv.lightness
       ChangeLight l -> hsl hsv.hue hsv.saturation l
       NewTitle NamedColors -> rgb 0 0 0
       NewTitle Mint ->rgb 152 255 152
       NewTitle Orange ->rgb 255 165 0
       NewTitle Lavander ->rgb 181 126 220
       NewTitle Pink  ->rgb 255 102 204
       NewTitle Cornflower -> rgb 100 149 234
       NewTitle Gold -> rgb 255 215 0
       NewTitle Fuchsia -> rgb 255 0 255
       NewTitle Plum -> rgb 221 160 221
       NewTitle Orchard -> rgb 133 193 14
       NewTitle Teal -> rgb 0 128 128
       Complement -> Color.complement model



toInt s =
  Result.withDefault 0 (String.toInt s)
strToFloat x =
  Result.withDefault 0 (String.toFloat x)
toRadians r =
 r*(180/pi)
toPcnt p = p/100
slider value msg =
  input [ Attr.type_ "range",
          Attr.value (toString value),
          Attr.min "0",
          Attr.max "255",
          Ev.onInput (toInt >> msg)
        ] []

hueslider value msg =
  input [  Attr.type_ "range",
           Attr.value (toString value),
           Attr.min "0",
           Attr.max "360",
           Ev.onInput (strToFloat >> degrees >> msg)
          ] []
slslider value msg =
    input [ Attr.type_ "range",
            Attr.value(toString value),
            Attr.min "0",
            Attr.max "100",
            Ev.onInput (strToFloat >> toPcnt >> msg)
    ] []

view model =
  let c = toRgb model
      hsv = toHsl model
      rgbStr = "rgb(" ++ toString c.red ++ ","
                      ++ toString c.green ++ ","
                      ++ toString c.blue  ++ ")"

      hexcolor = colorToHex(rgb c.red c.green c.blue)   
      hslColor = Color.toHsl(rgb c.red c.green c.blue)

  in
  div [Attr.style [("border", "1px solid blue")]]
      [ text "RGB AND HSL COLOR PICKER"
      , br [] []
      , br [] []
      , text "R:\t\t\t\t\t\t"
      , slider c.red ChangeRed
      , text(toString c.red ++ "   " ++ Round.round 3 (toFloat (c.red) * 1 / 255))
      , br [] []
      , text "G:\t\t\t\t\t\t"
      , slider c.green ChangeGreen
      , text(toString c.green ++ "  " ++ Round.round 3(toFloat (c.green) * 1/255))
      , br [] []
      , text "B:"
      , slider c.blue ChangeBlue
      , text(toString c.blue ++ "    " ++ Round.round 3 (toFloat (c.blue) * 1/255))
      , br [] []
      , text "H:"
      , hueslider (toRadians hsv.hue) ChangeHue
      , text(Round.round 2 hsv.hue ++ "      " ++ Round.round 0 (hsv.hue * 180/pi))
      , text(" degrees")
      , br [][]
      , text "S:"
      , slslider (hsv.saturation*100) ChangeSat
      , text(Round.round 2 (hsv.saturation*100))
      , text("%")
      , br [] []
      , text "L:"
      , slslider (hsv.lightness*100) ChangeLight
      , text(Round.round 0 (hsv.lightness*100))
      , text("%")
      , br [] []
      , text ("Value: " ++ toString model)
      , br [] []
      , node "center" []
         [text(toString hexcolor)]
      , div [Attr.align "center"]
          [button [onClick Complement] [text "Complement"]]
      , div [Attr.align "center"]
          [InputWidget.comboBox [] toString colors NamedColors
          |> Html.map NewTitle]
      , div [Attr.style [("backgroundColor", rgbStr),
                         ("width", "500px"),
                         ("height", "350px")]]
            []
      ]
main =
  beginnerProgram {model=model, view=view,
                   update=update}


