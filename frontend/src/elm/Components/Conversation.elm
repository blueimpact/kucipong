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
              , value = "kucipong は、我輩がご主人様（あなた）のために、ご主人様にピッタリなクーポンを見つけ出すサービスですにゃ"
              }
            ]
          ]
        }
      , submitType =
        InputConfirm
          { input = "OK"
          , label = "いいね！"
          }
      , next = always "area0"
      }
    )
  , ( "area0"
    , { question =
        { speaker = AI
        , feeling = Just FeelNormal
        , balloons =
          [ [ { ptag = PlainParagraph
              , value = "まずは、ご主人様の主な活動エリアの住所や建物名を教えてほしいですにゃ"
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
      , next = always "moreArea"
      }
    )
  , ( "moreArea"
    , { question =
        { speaker = AI
        , feeling = Just FeelNormal
        , balloons =
          [ [ { ptag = PlainParagraph
              , value = "職場付近など、他にもエリアを登録しますかにゃ？？"
              }
            ]
          ]
        }
      , submitType =
        SelectList
          { input = "yes"
          , options =
            [ { label = "登録する！"
              , value = "yes"
              }
            , { label = "大丈夫！"
              , value = "no"
              }
            ]
          }
      , next = \v ->
        case v of
          (SelectList { input }) ->
            if input == "yes"
               then "area"
               else "tags"
          _ -> "tags"
      }
    )
  , ( "area"
    , { question =
        { speaker = AI
        , feeling = Just FeelNormal
        , balloons =
          [ [ { ptag = PlainParagraph
              , value = "登録したいエリアを教えてほしいにゃん"
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
      , next = always "moreArea"
      }
    )
  , ( "tags"
    , { question =
        { speaker = AI
        , feeling = Just FeelNormal
        , balloons =
          [ [ { ptag = PlainParagraph
              , value = "ご主人様の興味がある分野を教えてほしいにゃん！！"
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
            , { label = "タグ5"
              , value = "5"
              }
            , { label = "タグ6"
              , value = "6"
              }
            , { label = "タグ7"
              , value = "7"
              }
            , { label = "タグ8"
              , value = "8"
              }
            , { label = "タグ9"
              , value = "9"
              }
            , { label = "タグ10"
              , value = "10"
              }
            ]
          , maximum = 6
          }
      , next = always "closing"
      }
    )
  , ( "closing"
    , { question =
        { speaker = AI
        , feeling = Just FeelNormal
        , balloons =
          [ [ { ptag = PlainParagraph
              , value = "これでご主人様にぴったりなクーポンを探せるにゃ！"
              }
            , { ptag = PlainParagraph
              , value = "これからも、ご主人様の行動にあわせて、吾輩も賢くなっていくから、期待しててにゃん！！"
              }
            ]
          ]
        }
      , submitType =
        InputConfirm
          { input = "OK"
          , label = "期待してるにゃん！"
          }
      , next = always "end"
      }
    )
  ]
