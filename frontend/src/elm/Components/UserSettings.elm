port module Components.UserSettings
  exposing
    ( Model
    , Location
    , Msg (OnLoadUserSettings)
    , init
    , update
    , subscriptions
    )
{-| Module to store user settings.
-}



-- PORTS

port askStoreUserSettings : Model -> Cmd msg
port onStoreUserSettings : (() -> msg) -> Sub msg


port askLoadUserSettings : () -> Cmd msg
port onLoadUserSettings : (Model -> msg) -> Sub msg



-- MODEL


type alias Model = Maybe
  { areas : List Location
  , tags : List Int
  , favorites : List Int
  , history : List Int
  }


type alias Location =
  { latitude : Float
  , longitude : Float
  }


init : (Model, Cmd Msg)
init =
  ( Nothing
  , askLoadUserSettings ()
  )



-- UPDATE


type Msg
  = OnStoreUserSettings
  | OnLoadUserSettings Model


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    OnStoreUserSettings ->
      ( model
      , Cmd.none
      )

    OnLoadUserSettings model' ->
      Debug.log "onload"
      ( model'
      , Cmd.none
      )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ onStoreUserSettings (always OnStoreUserSettings)
    , onLoadUserSettings OnLoadUserSettings
    ]
