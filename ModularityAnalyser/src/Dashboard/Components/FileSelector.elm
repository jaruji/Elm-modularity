module Dashboard.Components.FileSelector exposing (..)

import Color exposing (Color)
import Material.Icons.Navigation exposing (close)
import Css exposing (..)
import Css.Transitions exposing (easeInOut, transition, linear)
import Css.Animations exposing (keyframes, property)
import Svg exposing (svg)
import Html
-- import Html.Styled exposing (..)
-- import Html.Styled.Attributes exposing (css, href, src)
-- import Html.Styled.Events exposing (onClick)
import Svg.Styled exposing (toUnstyled, fromUnstyled)
import Time exposing (..)
import File exposing (File)
import File.Select as Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D


type alias Model =
  { 
    hover : Bool,
    files : List File
  }


init : (Model, Cmd Msg)
init =
  (Model False [], Cmd.none)


type Msg
  = Pick
  | DragEnter
  | DragLeave
  | GotFiles File (List File)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Pick ->
      ( model
      , Select.files ["image/*"] GotFiles
      )

    DragEnter ->
      ( { model | hover = True }
      , Cmd.none
      )

    DragLeave ->
      ( { model | hover = False }
      , Cmd.none
      )

    GotFiles file files ->
      ( { model
            | files = file :: files
            , hover = False
        }
      , Cmd.none
      )


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


view : Model -> Html Msg
view model =
  div
    [ style "border" (if model.hover then "6px dashed purple" else "6px dashed #ccc")
    , style "border-radius" "20px"
    , style "width" "480px"
    , style "height" "100px"
    , style "margin" "100px auto"
    , style "padding" "20px"
    , style "display" "flex"
    , style "flex-direction" "column"
    , style "justify-content" "center"
    , style "align-items" "center"
    , hijackOn "dragenter" (D.succeed DragEnter)
    , hijackOn "dragover" (D.succeed DragEnter)
    , hijackOn "dragleave" (D.succeed DragLeave)
    , hijackOn "drop" dropDecoder
    ]
    [ button [ onClick Pick ] [ text "Upload Images" ]
    , span [ style "color" "#ccc" ] [ text (Debug.toString model) ]
    ]


dropDecoder : D.Decoder Msg
dropDecoder =
  D.at ["dataTransfer","files"] (D.oneOrMore GotFiles File.decoder)


hijackOn : String -> D.Decoder msg -> Attribute msg
hijackOn event decoder =
  preventDefaultOn event (D.map hijack decoder)


hijack : msg -> (msg, Bool)
hijack msg =
  (msg, True)

getModel: (Model, Cmd Msg) -> Model
getModel (model, cmd) =
  model