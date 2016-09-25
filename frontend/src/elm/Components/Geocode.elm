port module Components.Geocode
  exposing
    ( Model
    , Msg
      ( OnGetGeocode
      , OnErrorGetGeocode
      )
    , Error (..)
    , init
    , update
    , subscriptions
    , view
    )
{-| Module to store user settings.
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Components.SubmitArea.Types exposing (..)
import Util exposing (cmdSucceed)



-- PORTS

port askGetGeocode : String -> Cmd msg
port onGetGeocode : (Location -> msg) -> Sub msg
port onErrorGetGeocode : (String -> msg) -> Sub msg
port onLoadGoogleMapApi : (String -> msg) -> Sub msg



-- MODEL


type alias Model =
  { address : String
  , location : Maybe Location
  , error : Maybe Error
  , key : Maybe String
  , input : String
  }


type Error
  = OK
  | ZERO_RESULTS
  | OVER_QUERY_LIMIT
  | REQUEST_DENIED
  | INVALID_REQUEST
  | UNKNOWN_ERROR


init : (Model, Cmd Msg)
init =
  ( { address = ""
    , location = Nothing
    , error = Nothing
    , key = Nothing
    , input = ""
    }
  , Cmd.none
  )



-- UPDATE


type Msg
  = AskGetGeocode
  | OnGetGeocode' Location
  | OnGetGeocode String Location
  | OnErrorGetGeocode String
  | OnLoadGoogleMapApi String
  | OnInputAddress String
  | OnUpdateMap


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    AskGetGeocode ->
      ( model
      , askGetGeocode model.address
      )

    OnGetGeocode' location ->
      ( { model
        | location = Just location
        }
      , cmdSucceed <| OnGetGeocode model.address location
      )

    OnGetGeocode _ _ ->
      ( model
      , Cmd.none
      )

    OnErrorGetGeocode err ->
      -- TODO
      ( { model
        | error = parseError err
        }
      , Cmd.none
      )

    OnLoadGoogleMapApi key ->
      ( { model
        | key = Just key
        }
      , Cmd.none
      )

    OnInputAddress input ->
      ( { model
        | input = input
        }
      , Cmd.none
      )

    OnUpdateMap ->
      ( { model
        | address = model.input
        }
      , Cmd.none
      )



-- VIEW


view : Model -> Html Msg
view model =
  Html.form
    [ class "getGeocodeArea submitArea"
    , onSubmit AskGetGeocode
    ]
    [ input
      [ type' "text"
      , defaultValue model.address
      , onInput OnInputAddress
      ]
      []
    , button
      [ type' "button"
      , class "getGeocodeArea-checkbutton"
      , onClick OnUpdateMap
      ]
      [ text "地図で確認する" ]
    , case (model.key, model.address) of
      (_, "") ->
        div [] []
      (Just key, address) ->
        div
          []
          [ iframe
            [ class "embeddedMap"
            , attribute "frameborder" "0"
            , attribute "allowfullscreen" ""
            , src <|
              "https://www.google.com/maps/embed/v1/place?key=" ++
              key ++
              "&q=" ++
              address
            ]
            []
          , button
            [ type' "submit"
            ]
            [ text "決定" ]
          ]
      _ ->
        div [] []
    ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ onLoadGoogleMapApi OnLoadGoogleMapApi
    , onGetGeocode OnGetGeocode'
    , onErrorGetGeocode OnErrorGetGeocode
    ]



-- HELPER FUNCTIONS


parseError : String -> Maybe Error
parseError str =
  case str of
    "OK" ->
      Just OK
    "ZERO_RESULTS" ->
      Just ZERO_RESULTS
    "OVER_QUERY_LIMIT" ->
      Just OVER_QUERY_LIMIT
    "REQUEST_DENIED" ->
      Just REQUEST_DENIED
    "INVALID_REQUEST" ->
      Just INVALID_REQUEST
    "UNKNOWN_ERROR" ->
      Just UNKNOWN_ERROR
    _ ->
      Nothing
