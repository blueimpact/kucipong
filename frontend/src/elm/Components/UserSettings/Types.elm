module Components.UserSettings.Types exposing (..)
{-| Types for `Components.UserSettings`

# Common

@docs UserSettings
@docs Location
-}


type alias UserSettings =
  { areas : List Location
  , tags : List Int
  , favorites : List Int
  , history : List Int
  }


type alias Location =
  { latitude : Float
  , longitude : Float
  }
