module Components.SubmitArea.Types exposing (..)

{-| Types for `Components.SubmitArea`

# Common

@docs InputField
@docs Location

# Input box

@docs InputConfirmConfig
@docs InputNameConfig
@docs InputNameInput
@docs FamilyName
@docs GivenName
@docs InputDateConfig
@docs InputDateTimeConfig
@docs InputRangeConfig
@docs InputPlaceConfig
@docs InputPlaceInput
@docs InputLocationConfig
@docs InputLocationInput

# Select box

@docs SelectListConfig
@docs SelectListOptionConfig
@docs SelectPhotoConfig
@docs MultiSelectConfig

# Text area

@docs TextAreaConfig
-}

import Date exposing (Date)
import Date.Extra as Date
import List.Extra as List
import Maybe.Extra as Maybe
import String


type alias Location =
  { latitude : Float
  , longitude : Float
  }


type InputField
  = InputNone
    -- Variations for string input.
  | InputString StringInput
  | InputEmail StringInput
  | InputPhoneNumber StringInput
  | InputPostalCode StringInput
  | InputConfirm InputConfirmConfig
  | InputName InputNameConfig
  | InputDate InputDateConfig
  | InputDateTime InputDateTimeConfig
  | InputRange InputRangeConfig
  | InputPlace InputPlaceConfig
  | InputLocation InputLocationConfig
  | SelectList SelectListConfig
  | SelectPhoto SelectPhotoConfig
  | MultiSelect MultiSelectConfig
  | TextArea TextAreaConfig


calcNextKey : InputField -> String
calcNextKey a =
  case a of
    InputNone ->
      ""

    InputString c ->
      c.input

    InputEmail c ->
      c.input

    InputPhoneNumber c ->
      c.input

    InputPostalCode c ->
      c.input

    InputConfirm c ->
      c.input

    InputName c ->
      c.input.family ++ " " ++ c.input.given

    InputDate c ->
      case c.input of
        Nothing ->
          ""

        Just d ->
          Date.toFormattedString "MM/dd/yyyy" d

    InputDateTime c ->
      case c.input of
        Nothing ->
          ""

        Just d ->
          Date.toFormattedString "MM/dd/yyyy HH:mm" d

    InputRange c ->
      toString c.input

    InputPlace c ->
      toString c.input.region

    InputLocation c ->
      toString c.input.address

    SelectList c ->
      c.input

    SelectPhoto c ->
      c.input

    MultiSelect c ->
      Maybe.withDefault "" <| List.head c.inputs

    TextArea c ->
      c.input


formatInputField : InputField -> String
formatInputField a =
  case a of
    InputNone ->
      ""

    InputString c ->
      c.input

    InputEmail c ->
      c.input

    InputPhoneNumber c ->
      c.input

    InputPostalCode c ->
      c.input

    InputConfirm c ->
      c.label

    InputName c ->
      c.input.family ++ " " ++ c.input.given

    InputDate c ->
      case c.input of
        Nothing ->
          ""

        Just d ->
          Date.toFormattedString "MM/dd/yyyy" d

    InputDateTime c ->
      case c.input of
        Nothing ->
          ""

        Just d ->
          Date.toFormattedString "MM/dd/yyyy HH:mm" d

    InputRange c ->
      toString c.input ++ "%"

    InputPlace c ->
      c.input.region
        ++ " "
        ++ c.input.locality
        ++ " "
        ++ c.input.streetAddress
        ++ " "
        ++ c.input.extendedAddress
        ++ " "
        ++ c.input.building

    InputLocation c ->
      c.input.address

    SelectList c ->
      let
        opts =
          c.options
      in
        Maybe.unwrap ""
          (\o -> o.label)
        <|
          List.find (\o -> o.value == c.input) opts

    SelectPhoto c ->
      c.input

    MultiSelect c ->
      String.join ", " <|
        List.map .label <|
          List.filter
            (\s -> List.member s.value c.inputs)
            c.selection

    TextArea c ->
      c.input


type alias InputConfirmConfig =
  { input : String
  , label : String
  }


type alias InputNameConfig =
  { input : InputNameInput }


type alias InputNameInput =
  { family : String
  , given : String
  }


type alias InputDateConfig =
  { input : Maybe Date }


type alias InputDateTimeConfig =
  { input : Maybe Date }


type alias InputRangeConfig =
  { input : Int
  , maxLabel : String
  , minLabel : String
  }


type alias InputPlaceConfig =
  { input : InputPlaceInput }


type alias InputPlaceInput =
  { region : String
  , locality : String
  , streetAddress : String
  , extendedAddress : String
  , building : String
  }


type alias InputLocationConfig =
  { input : InputLocationInput }


type alias InputLocationInput =
  { location : Location
  , address : String
  }


type alias FamilyName =
  String


type alias GivenName =
  String


type alias SelectListConfig =
  { input : String
  , options : List SelectListOptionConfig
  }


type alias SelectPhotoConfig =
  { input : String
  , options : List String
  }


type alias SelectListOptionConfig =
  { label : String
  , value : String
  }


type alias MultiSelectConfig =
  { inputs : List String
  , selection : List SelectListOptionConfig
  , maximum : Int
  }


type alias StringInput =
  { input : String }


type alias TextAreaConfig =
  { input : String
  , max : Int
  }
