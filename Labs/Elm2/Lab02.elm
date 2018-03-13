module CursorMover exposing (..)

import Html exposing (..)
import Mouse exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)

{- Main -}

main : Program Never Model Msg
main = Html.program
       { init = init
       , view = view
       , update = update
       , subscriptions = subscriptions
       }

{- Model -}

type alias Model = { x : Int
                   , y : Int }

initialModel : Model
initialModel = { x = 0
               , y = 0 }

init : ( Model, Cmd Msg )
init = ( initialModel, Cmd.none )

{- Update -}

type Msg = Position Int Int

update : Msg -> Model -> ( Model, Cmd a )
update msg model = case msg of
                   Position x y -> ({ model | x = x, y = y}, Cmd.none )

{- Subscriptions -}

subscriptions : Model -> Sub Msg
subscriptions model =
   Mouse.moves (\{ x, y } -> Position x y)

{- View -}

view : Model -> Html a
view model = let
      posX = toString model.x
      posY = toString model.y
        in svg [width "650",height "500"]
   [    ellipse [cx posX,cy posY, rx "60", ry "35", fill "grey"] []]
