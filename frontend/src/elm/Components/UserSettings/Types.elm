module Components.UserSettings.Types exposing (..)
{-| Types for `Components.UserSettings`

# Common

@docs UserSettings
-}

import Components.SubmitArea.Types exposing (InputLocationInput)


type alias UserSettings =
  { areas : List InputLocationInput
  , tags : List Int
  , favorites : List Int
  , history : List Int
  }
