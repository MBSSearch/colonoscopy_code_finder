module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, div, li, text, ul)
import Http
import Json.Decode as Decode exposing (Decoder, int, lazy, list, map, oneOf, string)
import Json.Decode.Pipeline exposing (required)


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
                Ok tree ->
                    ( Success tree, Cmd.none )

                Err error ->
                    ( Failure error, Cmd.none )


view : Model -> Html Msg
view model =
    case model of
        Loading ->
            div [] [ text "Loading..." ]

        Failure error ->
            div [] [ text <| "There's been an error: " ++ toString error ]

        Success tree ->
            viewNode tree.root


viewNode : Node -> Html Msg
viewNode node =
    div []
        [ text node.text
        , ul [] (map (\answer -> li [] [ viewAnswer answer ]) node.answers)
        ]


viewAnswer : Answer -> Html Msg
viewAnswer answer =
    let
        next =
            case answer.next of
                Question question ->
                    viewNode question

                Number number_ ->
                    div [] [ text <| "Item number " ++ String.fromInt number_ ]

                Error _ ->
                    div [] [ text "Unfunded" ]
    in
    div []
        [ text answer.text
        , next
        ]


getDecisionTree : Cmd Msg
getDecisionTree =
    Http.get
        { url = "https://s3-ap-southeast-2.amazonaws.com/static.mbssearch.com/colonoscopy_decision_tree.json"
        , expect = Http.expectJson GotDecisionTree decisionTreeDecoder
        }


type alias DecisionTree =
    { root : Node }


type alias Node =
    { text : String
    , answers : Answers
    }



-- The opaque type `Answers` allows us to _hide_ the recursion of nested
--  questions in a question's answer.
-- See https://github.com/elm/compiler/blob/master/hints/recursive-alias.md


type Answers
    = Answers (List Answer)



-- Because `Answers` hides `List Answer` from the rest of the code, in order to
-- `map` on the list we need implement our own function.
-- See this post for more info on implementing functions for opaque types:
-- https://medium.com/@ghivert/designing-api-in-elm-opaque-types-ce9d5f113033


map : (Answer -> a) -> Answers -> List a
map f (Answers l) =
    List.map f l


type alias Answer =
    { text : String
    , next : AnswerNext
    }


type AnswerNext
    = Question Node
    | Number Int
    | Error String


decisionTreeDecoder : Decoder DecisionTree
decisionTreeDecoder =
    Decode.succeed DecisionTree
        |> required "root" nodeDecoder


nodeDecoder : Decoder Node
nodeDecoder =
    Decode.succeed Node
        |> required "text" string
        |> required "answers" (Decode.map Answers (list (lazy (\_ -> answerDecoder))))


answerDecoder : Decoder Answer
answerDecoder =
    Decode.succeed Answer
        |> required "text" string
        |> required "next" (oneOf [ answerNextQuestionDecoder, answerNextNumberDecoder, answerNextErrorDecoder ])


answerNextNumberDecoder : Decoder AnswerNext
answerNextNumberDecoder =
    Decode.succeed Number
        |> required "item_number" int


answerNextQuestionDecoder : Decoder AnswerNext
answerNextQuestionDecoder =
    Decode.map Question nodeDecoder


answerNextErrorDecoder : Decoder AnswerNext
answerNextErrorDecoder =
    Decode.succeed Error
        |> required "item_number" string


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
