module Components.Conversation exposing
  ( Model
  , Msg
    ( LoadNextQuestion
    , OnLoadNextQuestion
    , OnErrorLoadNextQuestion
    )
  , init
  , update
  , subscriptions
  )
{-| Module for loading new conversations
-}


import Dict

import Components.Conversation.Types exposing (..)
import Components.SubmitArea.Types exposing (..)
import Components.TalkArea.Types exposing (..)
import Util exposing (cmdSucceed)


-- MODEL


type alias Model = Conversation


init : (Model, Cmd Msg)
init =
  let
    -- TODO: Also store current question
    model =
      { getTalkKey = "1"
      , dict = dict
      }
    cmd = case Dict.get model.getTalkKey model.dict of
      Nothing ->
        cmdSucceed <| OnErrorLoadNextQuestion
      Just q ->
        cmdSucceed <| OnLoadNextQuestion q
  in
    ( model
    , cmd
    )



-- UPDATE


type Msg
  = LoadNextQuestion InputField
  | OnLoadNextQuestion Question
  | OnErrorLoadNextQuestion


update : Msg -> Model -> (Model, Cmd Msg)
update message model =
  case message of
    LoadNextQuestion input ->
      let
        maybeNext =
          Dict.get model.getTalkKey model.dict
          `Maybe.andThen` \q ->
            Just (q.next input)
          `Maybe.andThen` \next ->
            Dict.get next model.dict
          `Maybe.andThen` \q' ->
            Just (q', next)
      in
        case maybeNext of
          Nothing ->
            ( model
            , cmdSucceed OnErrorLoadNextQuestion
            )

          Just (q, key) ->
            ( { model
              | getTalkKey = key
              }
            , cmdSucceed <| OnLoadNextQuestion q
            )

    OnLoadNextQuestion qustion ->
      ( model
      , Cmd.none
      )

    OnErrorLoadNextQuestion ->
      ( model
      , Cmd.none
      )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- DATA


dict : Dict.Dict String Question
dict = Dict.fromList
  [ ( "1"
    , { question =
        { speaker = AI
        , feeling = Just FeelNormal
        , balloons =
          [ [ { ptag = PlainParagraph
              , value = "Test"
              }
            ]
          ]
        }
      , submitType =
        InputString
          { input = "Hi!"
          }
      , next = always "2"
      }
    )
  , ( "2"
    , { question =
        { speaker = AI
        , feeling = Just FeelNormal
        , balloons =
          [ [ { ptag = PlainParagraph
              , value = "Test 2"
              }
            ]
          ]
        }
      , submitType =
        InputString
          { input = "Hi! 2"
          }
      , next = always "end"
      }
    )
  ]
