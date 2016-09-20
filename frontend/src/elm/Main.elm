import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)

import Components.Conversation as Conversation
import Components.SubmitArea as SubmitArea
import Components.SubmitArea.Types exposing (..)
import Components.TalkArea as TalkArea
import Components.UserSettings as UserSettings
import Util exposing (cmdSucceed)



-- APP


main : Program Never
main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { submitArea : SubmitArea.Model
  , talkArea : TalkArea.Model
  , conversation : Conversation.Model
  , userSettings : UserSettings.Model
  }


init : (Model, Cmd Msg)
init =
  let
    initialSubmitArea = SubmitArea.init
    initialTalkArea = TalkArea.init
    initialConversation = Conversation.init
    initialUserSettings = UserSettings.init
    model =
      { submitArea = modelOf initialSubmitArea
      , talkArea = modelOf initialTalkArea
      , conversation = modelOf initialConversation
      , userSettings = modelOf initialUserSettings
      }
    cmd = Cmd.batch
      [ Cmd.map TalkArea <| cmdOf initialTalkArea
      , Cmd.map SubmitArea <| cmdOf initialSubmitArea
      , Cmd.map UserSettings <| cmdOf initialUserSettings
      , Cmd.map Conversation <| cmdOf initialConversation
      ]
  in
    (model, cmd)



-- UPDATE


type Msg
  = SubmitArea SubmitArea.Msg
  | TalkArea TalkArea.Msg
  | UserSettings UserSettings.Msg
  | Conversation Conversation.Msg


update : Msg -> Model -> (Model, Cmd Msg)
update message model =
  case message of
    SubmitArea msg ->
      let
        (model', cmd') = SubmitArea.update msg model.submitArea
        newModel = { model | submitArea = model' }
      in
        case msg of
          SubmitArea.OnSubmit input ->
            ( newModel
            , Cmd.batch
              [ Cmd.map SubmitArea cmd'
              , Cmd.map TalkArea
                ( cmdSucceed <|
                  TalkArea.PushNewUserString <|
                  formatInputField input
                )
              , Cmd.map UserSettings
                ( cmdSucceed <|
                  UserSettings.AskStoreUserSetting
                    model.conversation.getTalkKey
                    input
                )
              , Cmd.map Conversation
                ( cmdSucceed <|
                  Conversation.LoadNextQuestion input
                )
              ]
            )
            -- TODO Store the answer

          _ ->
            ( newModel
            , Cmd.map SubmitArea cmd'
            )

    TalkArea msg ->
      let
        (model', cmd') = TalkArea.update msg model.talkArea
      in
        ( { model | talkArea = model' }
        , Cmd.map TalkArea cmd'
        )

    UserSettings msg ->
      let
        (model', cmd') = UserSettings.update msg model.userSettings
      in
        ( { model | userSettings = model' }
        , Cmd.batch
          [ Cmd.map UserSettings cmd'
          -- TODO
          -- , loadNewConversation model'
          ]
        )

    Conversation msg ->
      let
        (model', cmd') = Conversation.update msg model.conversation
        newModel =
          { model | conversation = model' }
      in
        case msg of
          Conversation.OnLoadNextQuestion q ->
            ( { newModel
              | submitArea = q.submitType
              }
            , Cmd.batch
              [ Cmd.map TalkArea
                (cmdSucceed <| TalkArea.PushTalkBlock q.question)
              ]
            )

          _ ->
            ( newModel
            , Cmd.map Conversation cmd'
            )



-- VIEW


view : Model -> Html Msg
view model =
  div [class "container"] [
    div [class "header"] [
      div [class "header_title"]
        [text "Kucipong"]
    ],
    div [class "body", id "js-body"] [
      App.map TalkArea (TalkArea.view model.talkArea)
    ],
    div [class "footer"] [
      App.map SubmitArea (SubmitArea.view model.submitArea)
    ]
  ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Sub.map TalkArea <| TalkArea.subscriptions model.talkArea
    , Sub.map SubmitArea <| SubmitArea.subscriptions model.submitArea
    ]



-- UTILS


modelOf : (a, b) -> a
modelOf = fst


cmdOf : (a, b) -> b
cmdOf = snd
