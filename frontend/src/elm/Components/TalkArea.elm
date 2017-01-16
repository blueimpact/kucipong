module Components.TalkArea exposing
  ( Model
  , Msg
    ( PushTalkBlock
    , PushNewAITalk
    , PushNewUserString
    )
  , update
  , init
  , view
  , subscriptions
  )

import Dom.Scroll as Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import List
import List.Extra as List
import Maybe.Extra exposing ((?), isJust, mapDefault)
import String
import Task

import Components.TalkArea.Types exposing (..)



-- MODEL


type alias Model = List TalkBlock


init : (Model, Cmd Msg)
init =
  ( []
  , Cmd.none
  )


showSpeaker : Speaker -> String
showSpeaker speaker =
  case speaker of
    AI -> "ai"
    User -> "user"


paragraphTagToClassName : ParagraphTag -> String
paragraphTagToClassName ptag =
  case ptag of
    -- TODO
    _ -> ""


showFeeling : Feeling -> String
showFeeling feeling =
  case feeling of
    FeelNormal -> "feelNormal"
    FeelBad -> "feelBad"
    FeelGood -> "feelGood"



-- UPDATE


type Msg
  = PushTalkBlock TalkBlock
  | PushNewAITalk TalkBlock
  | PushNewUserString String
  | None


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    PushTalkBlock tb ->
      ( case List.last model of
        Nothing ->
          push tb model

        Just {speaker, feeling, balloons} ->
          if speaker == tb.speaker && feeling == tb.feeling then
            push
              { tb
              | balloons = balloons ++ tb.balloons
              }
              <| List.init model ? []
          else
            push tb model
      , scrollToNewMsg
      )

    PushNewAITalk x ->
      ( model ++ List.singleton x
      , scrollToNewMsg
      )

    PushNewUserString str ->
      ( model ++ List.singleton
          { speaker = User
          , feeling = Nothing
          , balloons =
            [ [ { ptag = PlainParagraph
                , value = str
                }
              ]
            ]
          }
      , scrollToNewMsg
      )
    None ->
      ( model
      , Cmd.none
      )



-- VIEW


view : Model -> Html Msg
view model =
  div [class "output-area"]
    <| List.map renderTalkBlock model


renderTalkBlock : TalkBlock -> Html Msg
renderTalkBlock tb =
  div
    [ classList
      [ ("input-group", True)
      , (showSpeaker tb.speaker, True)
      ]
    ]
    [ div
      [ classList
        [ ("img", tb.speaker == AI)
        , (mapDefault "" showFeeling tb.feeling, isJust tb.feeling)
        ]
      ] []
    , div [class "message-area"]
      <| List.map renderTalkBody tb.balloons
    ]


renderTalkBody : TalkBody -> Html Msg
renderTalkBody tb =
  div [class "message-balloon"]
    <| List.map renderTalkParagraph tb


renderTalkParagraph : TalkParagraph -> Html Msg
renderTalkParagraph tp =
  p [ classList
    [ (paragraphTagToClassName tp.ptag, tp.ptag /= PlainParagraph)
    ]
  ] [text tp.value]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- HELPER


{-| Make simple user talk from string
-}
userTalk : String -> TalkBlock
userTalk talk =
  { speaker = User
  , feeling = Nothing
  , balloons = List.singleton
    <| List.map (TalkParagraph PlainParagraph)
    <| String.split "\n" talk
  }



{-| Push an element after last element of given List
    Note that this operation requires O(N) time.
-}
push : a -> List a -> List a
push x xs = xs ++ List.singleton x


scrollToNewMsg : Cmd Msg
scrollToNewMsg =
  Task.perform (always None) (always None) <| Dom.toBottom "js-body"
