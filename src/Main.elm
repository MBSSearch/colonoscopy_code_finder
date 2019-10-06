module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, div, text)
import Http
import Json.Decode exposing (Decoder, field, string)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type Model
    = Loading
    | Failure Http.Error
    | Success DecisionTree


type alias DecisionTree =
    String


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, getDecisionTree )


type Msg
    = GotDecisionTree (Result Http.Error DecisionTree)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        GotDecisionTree result ->
            case result of
                Ok value ->
                    ( Success value, Cmd.none )

                Err error ->
                    ( Failure error, Cmd.none )


view : Model -> Html Msg
view model =
    case model of
        Loading ->
            div [] [ text "Loading..." ]

        Failure error ->
            div [] [ text <| "There's been an error: " ++ toString error ]

        Success value ->
            div [] [ text <| "Loaded. I haven't figured out how to decode the tree yet, but the first question is: " ++ value ]


getDecisionTree : Cmd Msg
getDecisionTree =
    Http.get
        { url = "https://s3-ap-southeast-2.amazonaws.com/static.mbssearch.com/colonoscopy_decision_tree.json"
        , expect = Http.expectJson GotDecisionTree decisionTreeDecoder
        }


decisionTreeDecoder : Decoder DecisionTree
decisionTreeDecoder =
    field "root" (field "text" string)


toString : Http.Error -> String
toString error =
    case error of
        Http.BadUrl string ->
            "bad url" ++ string

        Http.Timeout ->
            "timeout"

        Http.NetworkError ->
            "network error"

        Http.BadStatus code ->
            "bad status" ++ String.fromInt code

        Http.BadBody string ->
            "bad body" ++ string
