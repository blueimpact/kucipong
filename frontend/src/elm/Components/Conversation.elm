module Components.Conversation exposing
  ( Model
  , Msg
    ( LoadInitialQuestion
    , LoadNextQuestion
    , OnLoadNextQuestion
    , OnErrorLoadNextQuestion
    , PutDefaultUserSettings
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
import Components.UserSettings.Types exposing (..)
import Util exposing (cmdSucceed)


-- MODEL


type alias Model = Conversation


init : (Model, Cmd Msg)
init =
  ( { getTalkKey = "hello"
    , dict = dict
    }
  , Cmd.none
  )



-- UPDATE


type Msg
  = LoadInitialQuestion
  | LoadNextQuestion InputField
  | OnLoadNextQuestion Question
  | OnErrorLoadNextQuestion
  | PutDefaultUserSettings UserSettings


update : Msg -> Model -> (Model, Cmd Msg)
update message model =
  case message of
    LoadInitialQuestion ->
      ( model
      , cmdSucceed <|
        case Dict.get model.getTalkKey model.dict of
          Nothing ->
            OnErrorLoadNextQuestion
          Just q ->
            OnLoadNextQuestion q
      )
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

    PutDefaultUserSettings settings ->
      ( putDefaultUserSettings settings model
      , cmdSucceed LoadInitialQuestion
      )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- HELPER FUNCTIONS


putDefaultUserSettings : UserSettings -> Model -> Model
putDefaultUserSettings settings model =
  { model
  | dict =
    Dict.update "tags"
      (Maybe.map
        (\tags ->
          { tags
          | submitType =
            case tags.submitType of
              MultiSelect c ->
                MultiSelect
                  { c
                  | inputs = List.map toString settings.tags
                  }
              a -> a
          }
        )
      )
      model.dict
  }



-- DATA


dict : Dict.Dict String Question
dict = Dict.fromList
  [ ( "hello"
    , { question =
        { speaker = AI
        , feeling = Just FeelNormal
        , balloons =
          [ [ { ptag = PlainParagraph
              , value = "kucipong は、私がご主人様（あなた）のために、ご主人様にピッタリなクーポンを見つけ出すサービスですにゃ"
              }
            ]
          ]
        }
      , submitType =
        InputConfirm
          { input = "OK"
          , label = "いいね！"
          }
      , next = always "area"
      }
    )
  , ( "area"
    , { question =
        { speaker = AI
        , feeling = Just FeelNormal
        , balloons =
          [ [ { ptag = PlainParagraph
              , value = "Area"
              }
            ]
          ]
        }
      , submitType =
        InputLocation
          { input =
            { location =
              { latitude = 0
              , longitude = 0
              }
            , address = ""
            }
          }
      , next = always "tags"
      }
    )
  , ( "tags"
    , { question =
        { speaker = AI
        , feeling = Just FeelNormal
        , balloons =
          [ [ { ptag = PlainParagraph
              , value = "Tags"
              }
            ]
          ]
        }
      , submitType =
        MultiSelect
          { inputs = []
          , selection =
            [ { label = "タグ1"
              , value = "1"
              }
            , { label = "タグ2"
              , value = "2"
              }
            , { label = "タグ3"
              , value = "3"
              }
            , { label = "タグ4"
              , value = "4"
              }
            ]
          , maximum = 2
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
