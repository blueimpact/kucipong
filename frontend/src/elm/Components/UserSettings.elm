port module Components.UserSettings
  exposing
    ( Model
    , Msg
      ( AskLoadUserSettings
      , OnLoadUserSettings
      , AskStoreUserSetting
      , OnStoreUserSetting
      )
    , init
    , update
    , subscriptions
    )
{-| Module to store user settings.
-}

import List
import List.Extra as List
import Result
import String

import Components.Conversation.Types exposing (..)
import Components.SubmitArea.Types exposing (..)
import Components.UserSettings.Types exposing (..)
import Util exposing (cmdSucceed)



-- PORTS

port askStoreUserSettings : { key : TalkKey, settings : Model } -> Cmd msg
port onStoreUserSettings : (TalkKey -> msg) -> Sub msg


port askLoadUserSettings : () -> Cmd msg
port onLoadUserSettings : ((Maybe UserSettings) -> msg) -> Sub msg



-- MODEL


type alias Model = UserSettings


init : (Model, Cmd Msg)
init =
  ( { areas = []
    , tags = []
    , favorites = []
    , history = []
    }
  , cmdSucceed AskLoadUserSettings
  )



-- UPDATE


type Msg
  = AskLoadUserSettings
  | OnLoadUserSettings (Maybe Model)
  | AskStoreUserSetting TalkKey InputField
  | OnStoreUserSetting TalkKey


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    AskLoadUserSettings ->
      ( model
      , askLoadUserSettings ()
      )
    OnLoadUserSettings Nothing ->
      ( model
      , Cmd.none
      )

    OnLoadUserSettings (Just model') ->
      ( model'
      , Cmd.none
      )

    AskStoreUserSetting key input ->
      let
        model' = updateUserSettings key input model
      in
        ( model'
        , askStoreUserSettings
          { key = key
          , settings = model'
          }
        )

    OnStoreUserSetting _ ->
      ( model
      , Cmd.none
      )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ onStoreUserSettings OnStoreUserSetting
    , onLoadUserSettings OnLoadUserSettings
    ]



-- HELPER FUNCTIONS


updateUserSettings : TalkKey -> InputField -> UserSettings -> UserSettings
updateUserSettings key input model =
  case key of
    "area0" ->
      let
        location = takeLocation input
      in
        { model
        | areas = Maybe.withDefault [location] <|
          List.setAt 0 location model.areas
        }
    "area" ->
      { model
      | areas =
        model.areas ++
        [ takeLocation input ]
      }
    "tags" ->
      { model
      | tags = takeTags input
      }
    _ -> model

takeLocation : InputField -> InputLocationInput
takeLocation input =
  case input of
    InputLocation { input } ->
      input
    _ ->
      { location =
        { latitude = 0
        , longitude = 0
        }
      , address = ""
      }

takeTags : InputField -> List Int
takeTags input =
  case input of
    MultiSelect { inputs } ->
      List.filterMap
        (String.toInt >> Result.toMaybe)
        inputs
    _ -> []
