module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, a, div, li, text, ul)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
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



-- The model is not good enough, need better modelling (ah, ah)


type Model
    = Loading
    | Failure Http.Error
    | Success DecisionModel


type alias DecisionModel =
    { tree : DecisionTree
    , selection : Selection
    , history : List Node
    }


type alias DecisionTree =
    { root : Node }


type alias Node =
    { text : String
    , answers : Answers
    }


type alias Answer =
    { text : String
    , next : AnswerNext
    }



-- The opaque type `Answers` allows us to _hide_ the recursion of nested
--  questions in a question's answer.
-- See https://github.com/elm/compiler/blob/master/hints/recursive-alias.md


type Answers
    = Answers (List Answer)


type AnswerNext
    = Question Node
    | Number Int
    | Error String


type alias Selection =
    AnswerNext


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, getDecisionTree )


type Msg
    = GotDecisionTree (Result Http.Error DecisionTree)
    | Select Answer
    | GoBack


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotDecisionTree result ->
            case result of
                Ok tree ->
                    ( Success <| DecisionModel tree (Question tree.root) [], Cmd.none )

                Err error ->
                    ( Failure error, Cmd.none )

        Select answer ->
            case model of
                Success decisionModel ->
                    case decisionModel.selection of
                        Question currentNode ->
                            let
                                newHistory =
                                    decisionModel.history ++ [ currentNode ]
                            in
                            case answer.next of
                                Question node ->
                                    ( Success
                                        { decisionModel
                                            | selection = Question node
                                            , history = newHistory
                                        }
                                    , Cmd.none
                                    )

                                Number itemNumber ->
                                    ( Success
                                        { decisionModel
                                            | selection = Number itemNumber
                                            , history = decisionModel.history ++ [ currentNode ]
                                        }
                                    , Cmd.none
                                    )

                                Error message ->
                                    ( Success
                                        { decisionModel
                                            | selection = Error message
                                            , history = decisionModel.history ++ [ currentNode ]
                                        }
                                    , Cmd.none
                                    )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GoBack ->
            case model of
                Success decisionModel ->
                    let
                        lastElement =
                            List.head <| List.reverse decisionModel.history

                        remainder =
                            List.reverse <| List.drop 1 <| List.reverse decisionModel.history
                    in
                    case lastElement of
                        Just node ->
                            ( Success
                                { decisionModel
                                    | selection = Question node
                                    , history = remainder
                                }
                            , Cmd.none
                            )

                        Nothing ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    case model of
        Loading ->
            div [] [ text "Loading..." ]

        Failure error ->
            div [] [ text <| "There's been an error: " ++ toString error ]

        Success decisionModel ->
            if List.isEmpty decisionModel.history then
                div [] [ viewSelection decisionModel.selection ]

            else
                div []
                    [ a [ href "#", onClick GoBack ] [ text "Back" ]
                    , viewSelection decisionModel.selection
                    ]


viewSelection : Selection -> Html Msg
viewSelection selection =
    case selection of
        Question node ->
            viewNode node

        Number itemNumber ->
            viewItem itemNumber

        Error message ->
            div [] [ text message ]


viewNode : Node -> Html Msg
viewNode node =
    div []
        [ text node.text
        , ul [] (map (\answer -> li [] [ viewAnswer answer ]) node.answers)
        ]


viewItem : Int -> Html Msg
viewItem itemNumber =
    div [] [ text <| String.fromInt itemNumber ]


viewAnswer : Answer -> Html Msg
viewAnswer answer =
    div []
        [ a
            [ href "#", onClick <| Select answer ]
            [ text answer.text ]
        ]


getDecisionTree : Cmd Msg
getDecisionTree =
    Http.get
        { url = "https://s3-ap-southeast-2.amazonaws.com/static.mbssearch.com/colonoscopy_decision_tree.json"
        , expect = Http.expectJson GotDecisionTree decisionTreeDecoder
        }



-- Because `Answers` hides `List Answer` from the rest of the code, in order to
-- `map` on the list we need implement our own function.
-- See this post for more info on implementing functions for opaque types:
-- https://medium.com/@ghivert/designing-api-in-elm-opaque-types-ce9d5f113033


map : (Answer -> a) -> Answers -> List a
map f (Answers l) =
    List.map f l


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
