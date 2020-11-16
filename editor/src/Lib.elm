module Lib exposing (..)

import Html exposing (Attribute, Html, br, text)
import Html.Attributes exposing (..)
import Html.Events exposing (keyCode, on)
import Json.Decode as JD
import Json.Decode as Json
import Json.Encode as JE

const : a -> b -> a
const x _ = x

updateAt : Int -> (a -> a) -> List a -> List a
updateAt a b c = case (a, b, c) of
    (0, f, (x::xs)) -> f x :: xs
    (_, _, []) -> []
    (i, f, (x::xs)) -> x :: updateAt (i - 1) f xs

flip : (a -> b -> c) -> b -> a -> c
flip f x y = f y x


onKeyCode : Int -> msg -> Attribute msg
onKeyCode kc onKeyCodeAction =
    on "keyup" <|
        Json.andThen
            (\keyCode ->
                if keyCode == kc then
                    Json.succeed onKeyCodeAction

                else
                    Json.fail (String.fromInt keyCode)
            )
            keyCode

onAnyKey : (Int -> Maybe msg) -> Attribute msg
onAnyKey f =
    on "keypress" <|
        Json.andThen
            (\keyCode ->
                case f keyCode of
                    Just x -> Json.succeed x
                    Nothing -> Json.fail (String.fromInt keyCode)
            )
            keyCode

insertAt : Int -> a -> List a -> List a
insertAt a b c = case (a, b, c) of
    (0, y, xs) -> y::xs
    (_, _, []) -> []
    (i, y, x::xs) -> x :: insertAt (i - 1) y xs

onUp : msg -> Attribute msg
onUp = onKeyCode 38

onDown : msg -> Attribute msg
onDown = onKeyCode 38


onContentEditableInput : (String -> msg) -> Attribute msg
onContentEditableInput tagger =
    Html.Events.stopPropagationOn "input"
        (Json.map (\x -> ( x, True )) (Json.map tagger innerText))

innerText : Json.Decoder String
innerText =
  Json.at ["target", "innerText"] Json.string

empty : Html msg
empty = text ""

when : Bool -> Html msg -> Html msg
when b x = if b then x else empty

data k = attribute ("data-" ++ k)

dataToggle = data "toggle"

dataTarget = data "target"

br_ = br [] []

