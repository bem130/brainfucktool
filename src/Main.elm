module Main exposing (main)

import Dict exposing (Dict)
import String
import Char
import Debug

import Browser
import Html exposing (Html, textarea)
import Html.Attributes exposing (attribute, value, rows, cols)

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
    replicate x "+"

encodeString : String -> String
encodeString s =
    let
        codes = textEncoder s
        numbers = List.map number codes
    in
    String.join ".>" numbers ++ ".>"

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
    "<" ++ moveLeft (n - 1)

bfGet : Int -> String
bfGet n =
    moveRight n ++ ">"

bfPushNum : Int -> String
bfPushNum n =
    number n ++ ">"

bfPushStr : String -> String
bfPushStr s =
    encodeString s ++ ">"

bfAdd : String
bfAdd =
    "/* add top two stack values */"

bfSub : String
bfSub =
    "/* subtract top two stack values */"

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
                        sPrime = appendCode ">" 1 state
                    in
                    { sPrime | env = newEnv }
                )
                initState
                letvars
    in
    List.foldl (flip processCmd) stateAfterVars cmds

main =
    textarea
        [ attribute "readonly" ""
        , value exampleProgram.code
        , rows 30
        , cols 50
        ]
        []

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
