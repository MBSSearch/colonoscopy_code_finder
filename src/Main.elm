module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, div, text)


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( init, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    {}


init : Model
init =
    {}


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )


view : Model -> Html Msg
view _ =
    div [] [ text "Hello world" ]
