module Page.Repl exposing (Model, Msg, init, update, view)

import Eval
import Html exposing (..)
import Html.Attributes exposing (autofocus, class, placeholder, rows, style, value)
import Html.Events exposing (onClick, onInput)
import Session
import Skeleton
import Types
import Value


type alias Model =
    { session : Session.Data
    , input : String
    , history : List Entry
    }


type alias Entry =
    { input : String
    , output : String
    , isError : Bool
    }


type Msg
    = InputChanged String
    | Submit


init : Session.Data -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , input = ""
      , history = []
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputChanged newInput ->
            ( { model | input = newInput }, Cmd.none )

        Submit ->
            if String.isEmpty (String.trim model.input) then
                ( model, Cmd.none )

            else
                let
                    result =
                        Eval.eval model.input

                    entry =
                        case result of
                            Ok val ->
                                { input = model.input
                                , output = Value.toString val
                                , isError = False
                                }

                            Err error ->
                                { input = model.input
                                , output = errorToString error
                                , isError = True
                                }
                in
                ( { model
                    | input = ""
                    , history = model.history ++ [ entry ]
                  }
                , Cmd.none
                )


errorToString : Types.Error -> String
errorToString error =
    case error of
        Types.ParsingError deadEnds ->
            "Parse error"

        Types.EvalError data ->
            Types.evalErrorKindToString data.error


view : Model -> Skeleton.Details Msg
view model =
    { title = "Elm REPL"
    , header = [ Skeleton.Text "REPL" ]
    , warning = Skeleton.NoProblems
    , attrs = []
    , kids =
        [ div [ class "repl" ]
            [ viewHistory model.history
            , viewInput model.input
            ]
        ]
    }


viewHistory : List Entry -> Html msg
viewHistory entries =
    div [ class "repl-history" ] (List.map viewEntry entries)


viewEntry : Entry -> Html msg
viewEntry entry =
    div [ class "repl-entry" ]
        [ div [ class "repl-input" ]
            [ span [ style "color" "#666" ] [ text "> " ]
            , text entry.input
            ]
        , div
            [ class "repl-output"
            , style "color"
                (if entry.isError then
                    "#cc0000"

                 else
                    "#060"
                )
            ]
            [ text entry.output ]
        ]


viewInput : String -> Html Msg
viewInput current =
    div []
        [ textarea
            [ placeholder "Enter an Elm expression..."
            , value current
            , onInput InputChanged
            , autofocus True
            , rows 3
            , style "width" "100%"
            , style "font-family" "monospace"
            , style "font-size" "14px"
            , style "padding" "8px"
            , style "box-sizing" "border-box"
            ]
            []
        , button
            [ onClick Submit
            , style "margin-top" "8px"
            , style "padding" "6px 16px"
            ]
            [ text "Evaluate" ]
        ]
