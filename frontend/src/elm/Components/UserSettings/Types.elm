module Components.UserSettings.Types exposing (..)
{-| Types for `Components.UserSettings`

# Common

@docs UserSettings
-}

import Components.SubmitArea.Types exposing (Location)


type alias UserSettings =
  { areas : List Location
  , tags : List Int
  , favorites : List Int
  , history : List Int
  }
