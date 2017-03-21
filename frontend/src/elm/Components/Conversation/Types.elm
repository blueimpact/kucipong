module Components.Conversation.Types exposing (..)

{-| Types for `Components.Conversation`

# Common

@docs Conversation
@docs Question
@docs TalkKey
-}

import Dict exposing (Dict)
import Components.SubmitArea.Types exposing (..)
import Components.TalkArea.Types exposing (..)


type alias Conversation =
  { getTalkKey : TalkKey
  , dict : Dict TalkKey Question
  }


type alias Question =
  { question : TalkBlock
  , submitType : InputField
  , next : InputField -> TalkKey
  }


type alias TalkKey =
  String
