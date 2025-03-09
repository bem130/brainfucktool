module Main exposing (main)

import Dict exposing (Dict)
import String
import Char
import Debug

import Browser
import Html exposing (Html, textarea, button, div, text, br)
import Html.Events exposing (onClick)
import Html.Attributes exposing (attribute, value, rows, cols, style)

flip : (a -> b -> c) -> b -> a -> c
flip func b a =
    func a b


replicate : Int -> String -> String
replicate n s =
    if n <= 0 then
        ""
    else
        s ++ replicate (n - 1) s

textEncoder : String -> List Int
textEncoder s =
    s
        |> String.toList
        |> List.map Char.toCode

number : Int -> String
number x =
    replicate x "+" ++ ">"

encodeString : String -> String
encodeString s =
    let
        codes = textEncoder s
        numbers = List.map number codes
    in
    String.join ">" numbers ++ ">"

moveLeft : Int -> String
moveLeft n =
    replicate n "<"
        ++ "[-]"
        ++ replicate n ">"
        ++ "["
        ++ replicate n "<"
        ++ "+"
        ++ replicate n ">"
        ++ "-"
        ++ "]"

moveRight : Int -> String
moveRight n =
    "[-]"
        ++ replicate n "<"
        ++ "["
        ++ replicate n ">"
        ++ "+"
        ++ replicate n "<"
        ++ "-"
        ++ "]"
        ++ replicate n ">"

copyRight : Int -> String
copyRight n =
    "[-]"
        ++ replicate n "<"
        ++ "["
        ++ replicate n ">"
        ++ "+>+<"
        ++ replicate n "<"
        ++ "-"
        ++ "]"
        ++ replicate (n + 1) ">"
        ++ "["
        ++ replicate (n + 1) "<"
        ++ "+"
        ++ replicate (n + 1) ">"
        ++ "-"
        ++ "]"

type Cmd
    = Get String
    | Set String
    | PushNum Int
    -- | PushStr String
    | Add
    | Sub

type alias CompilerState =
    { env : Dict String Int
    , nextCell : Int
    , code : String
    }

adrLocal : String -> CompilerState -> Int
adrLocal var state =
    case Dict.get var state.env of
        Just val ->
            state.nextCell - val

        Nothing ->
            Debug.todo ("Error: Undefined Variable Name: " ++ var)

initState : CompilerState
initState =
    { env = Dict.empty, nextCell = 0, code = "" }

appendCode : String -> Int -> CompilerState -> CompilerState
appendCode s n state =
    let
        nc = state.nextCell + n
    in
        { state | code = state.code ++ s ++ "\n", nextCell = nc }

bfSet : Int -> String
bfSet n =
    "set  " ++ "<" ++ moveLeft (n - 1)

bfGet : Int -> String
bfGet n =
    "get  " ++ copyRight n

bfPushNum : Int -> String
bfPushNum n =
    "push " ++ number n

bfPushStr : String -> String
bfPushStr s =
    "push "  ++ encodeString s

bfAdd : String
bfAdd =
    "add  " ++ "<[<+>-]"

bfSub : String
bfSub =
    "sub  "  ++ "<[<->-]"

processCmd : CompilerState -> Cmd -> CompilerState
processCmd state cmd =
    case cmd of
        Get var ->
            appendCode (bfGet (adrLocal var state)) 1 state

        Set var ->
            appendCode (bfSet (adrLocal var state)) (-1) state

        PushNum n ->
            appendCode (bfPushNum n) 1 state

        -- PushStr s ->
        --     appendCode (bfPushStr s) 1 state

        Add ->
            appendCode bfAdd (-1) state

        Sub ->
            appendCode bfSub (-1) state

scope : List String -> List Cmd -> CompilerState
scope letvars cmds =
    let
        stateAfterVars =
            List.foldl
                (\var state ->
                    let
                        idx = state.nextCell
                        newEnv = Dict.insert var idx state.env
                        sPrime = appendCode "let  >" 1 state
                    in
                    { sPrime | env = newEnv }
                )
                initState
                letvars
    in
    List.foldl (flip processCmd) stateAfterVars cmds


type alias Model =
    { sourceCode : String
    , compiledCode : String
    , error : Maybe String
    , parseResult : Maybe (List String, List Cmd)  -- 追加: パース結果を保存
    }


initialModel : Model
initialModel =
    let
        sampleCode = """// sample code
let tmp, result
push 1
push 2
set tmp
get tmp
add
get tmp
sub
set result
"""

    in
    case parseDSL sampleCode of
        Ok result ->
            let
                (vars, cmds) = result
                compiled = scope vars cmds
            in
            { sourceCode = sampleCode
            , compiledCode = compiled.code
            , error = Nothing
            , parseResult = Just result
            }
        Err err ->
            { sourceCode = sampleCode
            , compiledCode = sampleCode
            , error = Just err
            , parseResult = Nothing
            }
    -- { sourceCode = 
    -- , compiledCode = ""
    -- , error = Nothing
    -- , parseResult = Nothing
    -- }


type Msg
    = UpdateSource String

parseDSL : String -> Result String (List String, List Cmd)
parseDSL source =
    let
        lines =
            String.lines source
                |> List.map String.trim
                |> List.filter (\line -> not (String.isEmpty line) && not (String.startsWith "//" line))
        parseLetLine line =
            if String.startsWith "let" line then
                String.dropLeft 3 line
                    |> String.trim
                    |> String.split ","
                    |> List.map String.trim
                    |> List.filter (not << String.isEmpty)
                    |> Ok
            else
                Err "First line must start with 'let'"

        parseCmd line =
            case String.words line of
                ["push", num] ->
                    String.toInt num
                        |> Maybe.map PushNum
                        |> Result.fromMaybe "Invalid number in push command"
                ["get", var] ->
                    Ok (Get var)
                ["set", var] ->
                    Ok (Set var)
                ["add"] ->
                    Ok Add
                ["sub"] ->
                    Ok Sub
                _ ->
                    Err ("Invalid command: " ++ line)
        parseCommands commands =
            List.foldl 
                (\cmd accResult ->
                    accResult
                        |> Result.andThen
                            (\acc ->
                                parseCmd cmd
                                    |> Result.map (\parsedCmd -> parsedCmd :: acc)
                            )
                )
                (Ok [])
                commands
                |> Result.map List.reverse
    in
    case lines of
        firstLine :: rest ->
            parseLetLine firstLine
                |> Result.andThen
                    (\vars ->
                        parseCommands rest
                            |> Result.map (\cmds -> (vars, cmds))
                    )
        [] ->
            Err "Empty program"


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateSource newSource ->
            case parseDSL newSource of
                Ok result ->
                    let
                        (vars, cmds) = result
                        compiled = scope vars cmds
                    in
                    { model
                    | sourceCode = newSource
                    , compiledCode = compiled.code
                    , error = Nothing
                    , parseResult = Just result
                    }
                Err err ->
                    { model
                    | sourceCode = newSource
                    , compiledCode = newSource
                    , error = Just err
                    , parseResult = Nothing
                    }


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ text "DSLソース:"
            , br [] []
            , textarea
                [ value model.sourceCode
                , Html.Events.onInput UpdateSource
                , rows 15
                , style "width" "100%"
                ]
                []
            ]
        , case model.error of
            Just err ->
                div [ Html.Attributes.style "color" "red" ]
                    [ text ("エラー: " ++ err) ]
            Nothing ->
                text ""
        -- , case model.parseResult of
        --     Just (vars, cmds) ->
        --         div [ Html.Attributes.style "margin" "10px"
        --             , Html.Attributes.style "padding" "10px"
        --             , Html.Attributes.style "border" "1px solid #ccc"
        --             ]
        --             [ div []
        --                 [ text "変数定義:"
        --                 , div [ Html.Attributes.style "margin-left" "20px" ]
        --                     [ text (String.join ".." vars) ]
        --                 ]
        --             , div [ Html.Attributes.style "margin-top" "10px" ]
        --                 [ text "コマンド列:"
        --                 , div [ Html.Attributes.style "margin-left" "20px" ]
        --                     (List.map
        --                         (\cmd ->
        --                             div []
        --                                 [ text (case cmd of
        --                                     Get var -> "Get " ++ var
        --                                     Set var -> "Set " ++ var
        --                                     PushNum n -> "Push " ++ String.fromInt n
        --                                     Add -> "Add"
        --                                     Sub -> "Sub"
        --                                 )
        --                                 ]
        --                         )
        --                         cmds
        --                     )
        --                 ]
        --             ]
        --     Nothing ->
        --         text ""
        , div []
            [ text "生成されたBrainfuckコード:"
            , br [] []
            , textarea
                [ value model.compiledCode
                , attribute "readonly" ""
                , rows 15
                , style "width" "100%"
                ]
                []
            ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }

exampleProgram : CompilerState
exampleProgram =
    scope
        [ "a", "b" ]
        [ PushNum 1
        , Set "b"
        , PushNum 2
        , Set "a"
        , PushNum 3
        , Get "b"
        , PushNum 8
        , Set "b"
        ]
