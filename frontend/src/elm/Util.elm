module Util
  exposing
    ( onChange
    , onChangeInt
    , onInputInt
    , intValue
    , intProperty
    , cmdSucceed
    )


import Html exposing (Attribute)
import Html.Attributes exposing (property)
import Html.Events exposing (..)
import Json.Decode as Json
import Json.Encode as Encode
import String
import Task


onChange : (String -> msg) -> Attribute msg
onChange tagger =
  on "change" (Json.map tagger targetValue)


onChangeInt : Int -> (Int -> msg) -> Attribute msg
onChangeInt def tagger =
  onChange (tagger << Result.withDefault def << String.toInt)


onInputInt : Int -> (Int -> msg) -> Attribute msg
onInputInt def tagger =
  onInput (tagger << Result.withDefault def << String.toInt)


intValue : Int -> Attribute msg
intValue = intProperty "value"


intProperty : String -> Int -> Attribute msg
intProperty name int =
  property name (Encode.int int)


cmdSucceed : a -> Cmd a
cmdSucceed a =
  Task.perform identity identity (Task.succeed a)
