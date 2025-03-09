module Main.1 exposing (..)module BfInterpreter exposing (main)

import Browser
import Html exposing (Html, button, div, input, pre, text, ul, li)
import Html.Attributes exposing (placeholder, style, value)
import Html.Events exposing (onClick, onInput)
import Array exposing (Array)


-- モデル

type alias Model =
    { program : String
    , input : String
    , output : String
    , memory : Array Int
    , pointer : Int
    , programCounter : Int
    , bracketStack : List Int
    , state : State
    , memoryView: Int
    }


type State
    = Ready
    | Running
    | Finished
    | Error String


init : () -> ( Model, Cmd Msg )
init _ =
    ( { program = ""
        , input = ""
        , output = ""
        , memory = Array.repeat 30000 0
        , pointer = 0
        , programCounter = 0
        , bracketStack = []
        , state = Ready
        , memoryView = 20
        }
    , Cmd.none
    )


-- アップデート

type Msg
    = UpdateProgram String
    | UpdateInput String
    | Run
    | Reset
    | Step


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateProgram newProgram ->
            ( { model
                | program = newProgram
                , state = Ready
                , memory = Array.repeat 30000 0
                , pointer = 0
                , programCounter = 0
                , bracketStack = []
                , output = ""
                }
            , Cmd.none
            )

        UpdateInput newInput ->
            ( { model | input = newInput }, Cmd.none )

        Run ->
            let
                newModel =
                    { model
                    | state = Running
                    , memory = Array.repeat 30000 0
                    , pointer = 0
                    , programCounter = 0
                    , bracketStack = []
                    , output = ""
                    }
            in
            runUntilCompletion newModel

        Reset ->
            ( { model
                | state = Ready
                , memory = Array.repeat 30000 0
                , pointer = 0
                , programCounter = 0
                , bracketStack = []
                , output = ""
                }
            , Cmd.none
            )

        Step ->
            let
                newModel =
                    if model.state == Ready then
                        { model | state = Running }
                    else
                        model
            in
            runStep newModel


runUntilCompletion : Model -> ( Model, Cmd Msg )
runUntilCompletion model =
    case model.state of
        Running ->
            let
                newModel =
                    runStep model |> Tuple.first
            in
            if newModel.state == Running then
                runUntilCompletion newModel
            else
                ( newModel, Cmd.none )

        _ ->
            ( model, Cmd.none )


runStep : Model -> ( Model, Cmd Msg )
runStep model =
    if model.programCounter >= String.length model.program then
        ( { model | state = Finished }, Cmd.none )
    else
        let
            currentChar =
                String.slice model.programCounter (model.programCounter + 1) model.program

            updatedModel =
                case currentChar of
                    ">" ->
                        { model | pointer = model.pointer + 1, programCounter = model.programCounter + 1 }

                    "<" ->
                        { model | pointer = max 0 (model.pointer - 1), programCounter = model.programCounter + 1 }

                    "+" ->
                        let
                            currentValue =
                                Array.get model.pointer model.memory |> Maybe.withDefault 0

                            newValue =
                                modBy 256 (currentValue + 1)
                        in
                        { model
                        | memory = Array.set model.pointer newValue model.memory
                        , programCounter = model.programCounter + 1
                        }

                    "-" ->
                        let
                            currentValue =
                                Array.get model.pointer model.memory |> Maybe.withDefault 0

                            newValue =
                                modBy 256 (if currentValue - 1 < 0 then currentValue + 255 else currentValue - 1)
                        in
                        { model
                        | memory = Array.set model.pointer newValue model.memory
                        , programCounter = model.programCounter + 1
                        }

                    "." ->
                        let
                            currentValue =
                                Array.get model.pointer model.memory |> Maybe.withDefault 0

                            char =
                                Char.fromCode currentValue |> String.fromChar
                        in
                        { model
                        | output = model.output ++ char
                        , programCounter = model.programCounter + 1
                        }

                    "," ->
                        let
                            inputValue =
                                String.uncons model.input
                                    |> Maybe.map (\( c, rest ) -> ( Char.toCode c, rest ))
                                    |> Maybe.withDefault ( 0, model.input )
                        in
                        { model
                        | memory = Array.set model.pointer (Tuple.first inputValue) model.memory
                        , input = Tuple.second inputValue
                        , programCounter = model.programCounter + 1
                        }

                    "[" ->
                        let
                            currentValue =
                                Array.get model.pointer model.memory |> Maybe.withDefault 0
                        in
                        if currentValue == 0 then
                            findMatchingBracket model model.programCounter 1
                        else
                            { model
                            | bracketStack = model.programCounter :: model.bracketStack
                            , programCounter = model.programCounter + 1
                            }

                    "]" ->
                        let
                            currentValue =
                                Array.get model.pointer model.memory |> Maybe.withDefault 0
                        in
                        case model.bracketStack of
                            [] ->
                                { model | state = Error "Unmatched closing bracket" }

                            openBracket :: rest ->
                                if currentValue == 0 then
                                    { model
                                    | bracketStack = rest
                                    , programCounter = model.programCounter + 1
                                    }
                                else
                                    { model | programCounter = openBracket + 1 }

                    _ ->
                        -- 無視する文字（コメント）
                        { model | programCounter = model.programCounter + 1 }
        in
        ( updatedModel, Cmd.none )


findMatchingBracket : Model -> Int -> Int -> Model
findMatchingBracket model pos depth =
    let
        nextPos =
            pos + 1
    in
    if nextPos >= String.length model.program then
        { model | state = Error "Unmatched opening bracket" }
    else
        let
            char =
                String.slice nextPos (nextPos + 1) model.program
        in
        case char of
            "[" ->
                findMatchingBracket model nextPos (depth + 1)

            "]" ->
                if depth == 1 then
                    { model | programCounter = nextPos + 1 }
                else
                    findMatchingBracket model nextPos (depth - 1)

            _ ->
                findMatchingBracket model nextPos depth


-- ビュー

view : Model -> Html Msg
view model =
    div [ style "padding" "20px"
        , style "font-family" "monospace"
        , style "background-color" "#1e1e1e"
        , style "color" "#d4d4d4"
        , style "min-height" "100vh"
        ]
        [ div [ style "margin-bottom" "10px" ]
            [ div [ style "margin-bottom" "5px" ] [ text "Brainfuckプログラム:" ]
            , input
                [ value model.program
                , onInput UpdateProgram
                , placeholder "Brainfuckコードを入力してください"
                , style "width" "100%"
                , style "padding" "5px"
                , style "font-family" "monospace"
                , style "background-color" "#2d2d2d"
                , style "color" "#d4d4d4"
                , style "border" "1px solid #3d3d3d"
                ]
                []
            ]
        , div [ style "margin-bottom" "10px" ]
            [ div [ style "margin-bottom" "5px" ] [ text "入力:" ]
            , input
                [ value model.input
                , onInput UpdateInput
                , placeholder "入力を提供"
                , style "width" "100%"
                , style "padding" "5px"
                , style "font-family" "monospace"
                , style "background-color" "#2d2d2d"
                , style "color" "#d4d4d4"
                , style "border" "1px solid #3d3d3d"
                ]
                []
            ]
        , div [ style "margin-bottom" "10px" ]
            [ button 
                [ onClick Run
                , style "margin-right" "10px"
                , style "background-color" "#3d3d3d"
                , style "color" "#d4d4d4"
                , style "border" "1px solid #4d4d4d"
                , style "padding" "5px 10px"
                ] [ text "実行" ]
            , button 
                [ onClick Step
                , style "margin-right" "10px"
                , style "background-color" "#3d3d3d"
                , style "color" "#d4d4d4"
                , style "border" "1px solid #4d4d4d"
                , style "padding" "5px 10px"
                ] [ text "ステップ実行" ]
            , button 
                [ onClick Reset
                , style "background-color" "#3d3d3d"
                , style "color" "#d4d4d4"
                , style "border" "1px solid #4d4d4d"
                , style "padding" "5px 10px"
                ] [ text "リセット" ]
            ]
        , div [ style "margin-bottom" "10px" ]
            [ div [ style "margin-bottom" "5px" ] [ text "状態:" ]
            , div [ style "background-color" "#2d2d2d", style "padding" "5px" ]
                [ text <|
                    case model.state of
                        Ready ->
                            "準備完了"

                        Running ->
                            "実行中"

                        Finished ->
                            "完了"

                        Error msg ->
                            "エラー: " ++ msg
                ]
            ]
        , div [ style "margin-bottom" "10px" ]
            [ div [ style "margin-bottom" "5px" ] [ text "出力:" ]
            , pre
                [ style "background-color" "#2d2d2d"
                , style "padding" "10px"
                , style "border" "1px solid #3d3d3d"
                , style "white-space" "pre-wrap"
                , style "min-height" "50px"
                ]
                [ text model.output ]
            ]
        , div []
            [ div [ style "margin-bottom" "5px" ] [ text "メモリー:" ]
            , div [ style "display" "flex", style "overflow-x" "auto", style "background-color" "#2d2d2d", style "padding" "10px" ]
                (List.range 0 (model.memoryView - 1)
                    |> List.map
                        (\i ->
                            let
                                value =
                                    Array.get i model.memory |> Maybe.withDefault 0

                                isCurrentPointer =
                                    i == model.pointer

                                cellStyle =
                                    if isCurrentPointer then
                                        [ style "background-color" "#707000"
                                        , style "font-weight" "bold"
                                        ]
                                    else
                                        []
                                liStyle =
                                    [ style "height" "17px"
                                    ]
                            in
                            div
                                (
                                    [ style "width" "30px"
                                    , style "height" "fit-content"
                                    , style "min-width" "20px"
                                    , style "border" "1px solid #4d4d4d"
                                    , style "margin-right" "5px"
                                    , style "display" "flex"
                                    , style "align-items" "center"
                                    , style "justify-content" "center"
                                    , style "background-color" "#3d3d3d"
                                    ] ++ cellStyle
                                )
                                [
                                    ul
                                    [ style "padding-left" "0px"
                                    , style "list-style" "none"
                                    , style "margin" "0px"
                                    ]
                                    [ li liStyle [text (String.fromInt value)]
                                    , li liStyle [text (Char.fromCode value |> String.fromChar)]
                                    ]
                                ]
                        )
                )
            ]
        , div [ style "margin-top" "20px" ]
            [ div [ style "margin-bottom" "5px" ] [ text "サンプルプログラム:" ]
            , div [ style "display" "flex", style "flex-direction" "column", style "gap" "5px" ]
                [ div 
                    [ style "cursor" "pointer"
                    , style "padding" "5px"
                    , style "background-color" "#2d2d2d"
                    , style "border" "1px solid #3d3d3d"
                    , onClick (UpdateProgram "+++++++++[>++++++++<-]>.<++++++[>++++++<-]>-.+++++++..+++.")
                    ] 
                    [ text "Hello" ]
                , div 
                    [ style "cursor" "pointer"
                    , style "padding" "5px"
                    , style "background-color" "#2d2d2d"
                    , style "border" "1px solid #3d3d3d"
                    , onClick (UpdateProgram "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.")
                    ] 
                    [ text "Hello World" ]
                , div 
                    [ style "cursor" "pointer"
                    , style "padding" "5px"
                    , style "background-color" "#2d2d2d"
                    , style "border" "1px solid #3d3d3d"
                    , onClick (UpdateProgram ">,[>,]<[.<]")
                    ] 
                    [ text "入力を逆順に出力" ]
                ]
            ]
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }