module Components.SubmitArea
  exposing
    ( Model
    , Msg (OnSubmit)
    , init
    , update
    , view
    , subscriptions
    )

import Date
import Date.Extra as Date
import Date.Extra.Facts as Date
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra as List
import Regex

import Components.Geocode as Geocode
import Components.SubmitArea.Types exposing (..)
import Util exposing
  ( onChange
  , onChangeInt
  , onInputInt
  , intValue
  , cmdSucceed
  )



-- MODEL


type alias Model =
  { inputField : InputField
  , geocode : Geocode.Model
  }


init : (Model, Cmd Msg)
init =
  let
    (mGeocode, cGeocode) = Geocode.init
  in
    ( { inputField = InputNone
      , geocode = mGeocode
      }
    , Cmd.batch
      [ Cmd.none
      , Cmd.map Geocode cGeocode
      ]
    )



-- UPDATE


type Msg
  = OnSubmit InputField
  | OnInputString StringInput String
  | OnInputEmail StringInput String
  | OnInputPhoneNumber StringInput String
  | OnInputPostalCode StringInput String
  | OnInputFamilyName InputNameConfig String
  | OnInputGivenName InputNameConfig String
  | OnSelectDateYear InputDateConfig Int
  | OnSelectDateMonth InputDateConfig Int
  | OnSelectDateDay InputDateConfig Int
  | OnSelectDateTimeYear InputDateTimeConfig Int
  | OnSelectDateTimeMonth InputDateTimeConfig Int
  | OnSelectDateTimeDay InputDateTimeConfig Int
  | OnSelectDateTimeHour InputDateTimeConfig Int
  | OnSelectDateTimeMinute InputDateTimeConfig Int
  | OnInputRange InputRangeConfig Int
  | OnInputPlaceRegion InputPlaceConfig String
  | OnInputPlaceLocality InputPlaceConfig String
  | OnInputPlaceStreetAddress InputPlaceConfig String
  | OnInputPlaceExtendedAddress InputPlaceConfig String
  | OnInputPlaceBuilding InputPlaceConfig String
  | OnSelectList SelectListConfig String
  | OnSelectPhoto SelectPhotoConfig String
  | OnCheckMultiSelect MultiSelectConfig String Bool
  | OnInputTextArea TextAreaConfig String
  | OnInputLocation InputLocationConfig String
  | Geocode Geocode.Msg


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    OnSubmit input ->
      ( { model
        | inputField = InputNone
        }
      , Cmd.none
      )

    OnInputString c val ->
      ( { model
        | inputField = InputString
          { c | input = val }
        }
      , Cmd.none
      )

    OnInputEmail c val ->
      ( { model
        | inputField = InputEmail
          { c | input = val }
        }
      , Cmd.none
      )

    OnInputPhoneNumber c val ->
      ( { model
        | inputField = InputPhoneNumber
          { c | input = val }
        }
      , Cmd.none
      )

    OnInputPostalCode c val ->
      ( { model
        | inputField = InputPostalCode
          { c | input = val }
        }
      , Cmd.none
      )

    OnInputFamilyName c fam ->
      ( { model
        | inputField = InputName
          { c | input =
            { family = fam
            , given = c.input.given
            }
          }
        }
      , Cmd.none
      )

    OnInputGivenName c given ->
      ( { model
        | inputField = InputName
          { c | input =
            { family = c.input.family
            , given = given
            }
          }
        }
      , Cmd.none
      )

    OnInputRange c v ->
      ( { model
        | inputField = InputRange
          { c | input = v
          }
        }
      , Cmd.none
      )

    OnInputPlaceRegion c val ->
      let
        input = c.input
      in
      ( { model
        | inputField = InputPlace
          { c
          | input =
            { input
            | region = val
            }
          }
        }
      , Cmd.none
      )

    OnInputPlaceLocality c val ->
      let
        input = c.input
      in
      ( { model
        | inputField = InputPlace
          { c
          | input =
            { input
            | locality = val
            }
          }
        }
      , Cmd.none
      )

    OnInputPlaceStreetAddress c val ->
      let
        input = c.input
      in
      ( { model
        | inputField = InputPlace
          { c
          | input =
            { input
            | streetAddress = val
            }
          }
        }
      , Cmd.none
      )

    OnInputPlaceExtendedAddress c val ->
      let
        input = c.input
      in
      ( { model
        | inputField = InputPlace
          { c
          | input =
            { input
            | extendedAddress = val
            }
          }
        }
      , Cmd.none
      )

    OnInputPlaceBuilding c val ->
      let
        input = c.input
      in
      ( { model
        | inputField = InputPlace
          { c
          | input =
            { input
            | building = val
            }
          }
        }
      , Cmd.none
      )

    OnSelectDateYear c year ->
      let
        input = c.input
      in
        ( { model
          | inputField = InputDate
            { c | input =
              Maybe.map
                (\i ->
                  Date.fromCalendarDate
                    year
                    (Date.month i)
                    (Date.day i)
                )
                input
            }
          }
        , Cmd.none
        )

    OnSelectDateMonth c month ->
      let
        input = c.input
      in
        ( { model
          | inputField = InputDate
            { c | input =
              Maybe.map
                (\i ->
                  Date.fromCalendarDate
                    (Date.year i)
                    (Date.monthFromMonthNumber month)
                    (Date.day i)
                )
                input
            }
          }
        , Cmd.none
        )

    OnSelectDateDay c day ->
      let
        input = c.input
      in
        ( { model
          | inputField = InputDate
            { c | input =
              Maybe.map
                (\i ->
                  Date.fromCalendarDate
                    (Date.year i)
                    (Date.month i)
                    day
                )
                input
            }
          }
        , Cmd.none
        )

    OnSelectDateTimeYear c val ->
      let
        input = c.input
      in
        ( { model
          | inputField = InputDateTime
            { c | input =
              Maybe.map
                (\i ->
                  Date.fromParts
                    val
                    (Date.month i)
                    (Date.day i)
                    (Date.hour i)
                    (Date.minute i)
                    (Date.second i)
                    (Date.millisecond i)
                )
                input
            }
          }
        , Cmd.none
        )

    OnSelectDateTimeMonth c val ->
      let
        input = c.input
      in
        ( { model
          | inputField = InputDateTime
            { c | input =
              Maybe.map
                (\i ->
                  Date.fromParts
                    (Date.year i)
                    (Date.monthFromMonthNumber val)
                    (Date.day i)
                    (Date.hour i)
                    (Date.minute i)
                    (Date.second i)
                    (Date.millisecond i)
                )
                input
            }
          }
        , Cmd.none
        )

    OnSelectDateTimeDay c val ->
      let
        input = c.input
      in
        ( { model
          | inputField = InputDateTime
            { c | input =
              Maybe.map
                (\i ->
                  Date.fromParts
                    (Date.year i)
                    (Date.month i)
                    val
                    (Date.hour i)
                    (Date.minute i)
                    (Date.second i)
                    (Date.millisecond i)
                )
                input
            }
          }
        , Cmd.none
        )

    OnSelectDateTimeHour c val ->
      let
        input = c.input
      in
        ( { model
          | inputField = InputDateTime
            { c | input =
              Maybe.map
                (\i ->
                  Date.fromParts
                    (Date.year i)
                    (Date.month i)
                    (Date.day i)
                    val
                    (Date.minute i)
                    (Date.second i)
                    (Date.millisecond i)
                )
                input
            }
          }
        , Cmd.none
        )

    OnSelectDateTimeMinute c val ->
      let
        input = c.input
      in
        ( { model
          | inputField = InputDateTime
            { c | input =
              Maybe.map
                (\i ->
                  Date.fromParts
                    (Date.year i)
                    (Date.month i)
                    (Date.day i)
                    (Date.hour i)
                    val
                    (Date.second i)
                    (Date.millisecond i)
                )
                input
            }
          }
        , Cmd.none
        )

    OnSelectList c val ->
      ( { model
        | inputField = SelectList
          { c | input = val }
        }
      , Cmd.none
      )

    OnSelectPhoto c val ->
      ( { model
        | inputField = SelectPhoto
          { c | input = val }
        }
      , Cmd.none
      )

    OnCheckMultiSelect c val checked ->
      ( { model
        | inputField = MultiSelect
          { c
          | inputs =
            if checked
            then
              (Debug.log "val" val) :: c.inputs
            else
              List.filter ((/=) val) c.inputs
          }
        }
      , Cmd.none
      )

    OnInputTextArea c val ->
      ( { model
        | inputField = TextArea
          { c | input = val }
        }
      , Cmd.none
      )

    OnInputLocation c val ->
      let
        input = c.input
      in
        ( { model
          | inputField = InputLocation
            { c | input =
              { input
              | address = val
              }
            }
          }
        , Cmd.none
        )

    Geocode msg ->
      let
        (model', cmd') = Geocode.update msg model.geocode
        newModel =
          ( { model
            | geocode = model'
            }
          )
        newCmd = Cmd.map Geocode cmd'
      in
        case msg of
          Geocode.OnGetGeocode address location ->
            ( newModel
            , Cmd.batch
              [ cmdSucceed <| OnSubmit <|
                InputLocation
                  { input =
                    { location = location
                    , address = address
                    }
                  }
              , newCmd
              ]
            )

          _ ->
            ( newModel
            , newCmd
            )



-- VIEW


view : Model -> Html Msg
view model =
  case model.inputField of
    InputNone ->
      renderInputNone

    InputString config ->
      renderInputString config

    InputDate config ->
      renderInputDate config

    InputDateTime config ->
      renderInputDateTime config

    InputEmail config ->
      renderInputEmail config

    InputName config ->
      renderInputName config

    InputPhoneNumber config ->
      renderInputPhoneNumber config

    InputPostalCode config ->
      renderInputPostalCode config

    InputConfirm config ->
      renderInputConfirm config

    InputRange config ->
      renderInputRange config

    InputPlace config ->
      renderInputPlace config

    InputLocation config ->
      renderInputLocation model config

    SelectList config ->
      renderSelectList config

    SelectPhoto config ->
      renderSelectPhoto config

    MultiSelect config ->
      renderMultiSelect config

    TextArea config ->
      renderTextArea config


renderInputNone : Html Msg
renderInputNone =
  Html.form
    [ class "submitArea"
    , id "inputNone"
    ]
    []


renderInputStringHelper : (StringInput -> String -> Msg) -> String -> StringInput -> Html Msg
renderInputStringHelper toMsg t c =
  Html.form
    [ class "submitArea"
    , onSubmit (OnSubmit (InputString c))
    ]
    [ div [class "control-group inputString"]
      [ input
        [ type' t
        , value c.input
        , onInput (toMsg c)
        ]
        []
      ]
    , div [class "submit"]
      [ button
        [ class "btn default"
        , type' "submit"
        ]
        [ text "送信する"
        ]
      ]
    ]


renderInputString : StringInput -> Html Msg
renderInputString = renderInputStringHelper OnInputString "text"


renderInputEmail : StringInput -> Html Msg
renderInputEmail = renderInputStringHelper OnInputEmail "email"


renderInputPhoneNumber : StringInput -> Html Msg
renderInputPhoneNumber = renderInputStringHelper OnInputPhoneNumber "tel"


renderInputPostalCode : StringInput -> Html Msg
renderInputPostalCode = renderInputStringHelper OnInputPostalCode "text"


renderInputConfirm : InputConfirmConfig -> Html Msg
renderInputConfirm c =
  Html.form
    [ class "submitArea"
    , onSubmit (OnSubmit (InputConfirm c))
    ]
    [ div [class "submit"]
      [ button
        [ class "btn default"
        , type' "submit"
        ]
        [ text c.label ]
      ]
    ]


renderInputName : InputNameConfig -> Html Msg
renderInputName c =
  Html.form
    [ class "submitArea"
    , onSubmit (OnSubmit (InputName c))
    ]
    [ div [class "control-group inputName"]
      [ input
        [ type' "text"
        , value c.input.family
        , onInput (OnInputFamilyName c)
        ]
        []
      , input
        [ type' "text"
        , value c.input.given
        , onInput (OnInputGivenName c)
        ]
        []
      ]
    , div [class "submit"]
      [ button
        [ class "btn default"
        , type' "submit"
        ]
        [ text "送信する"
        ]
      ]
    ]


renderInputDate : InputDateConfig -> Html Msg
renderInputDate c =
  Html.form
    [ class "submitArea"
    , onSubmit (OnSubmit (InputDate c))
    ]
    [ div [class "control-group inputDate"]
      [ select
        [ onChangeInt 2000 (OnSelectDateYear c)
        , class "form-control"
        ]
        (List.map
          (\y ->
            option
              [ intValue y
              , selected <| Maybe.map Date.year c.input == Just y
              ]
              [ text (toString y ++ "年")
              ]
          )
          (List.unfoldr
            (\b -> if b == 1900
              then Nothing
              else Just (b, b-1)
            )
            2016
          )
        )
      , select
        [ onChangeInt 1 (OnSelectDateMonth c)
        , class "form-control"
        ]
        (List.map
          (\m ->
            option
              [ intValue m
              , selected <| Maybe.map Date.monthNumber c.input == Just m
              ]
              [ text (toString m ++ "月")
              ]
          )
          [1..12]
        )
      , select
        [ onChangeInt 1 (OnSelectDateDay c)
        , class "form-control"
        ]
        (List.map
          (\d ->
            option
              [ intValue d
              , selected <| Maybe.map Date.day c.input == Just d
              ]
              [ text (toString d ++ "日")
              ]
          )
          [1..31]
        )
      ]
    , div [class "submit"]
      [ button
        [ class "btn default"
        , type' "submit"
        ]
        [ text "送信する"
        ]
      ]
    ]


renderInputDateTime : InputDateTimeConfig -> Html Msg
renderInputDateTime c =
  Html.form
    [ class "submitArea"
    , onSubmit (OnSubmit (InputDateTime c))
    ]
    [ div [class "control-group inputDateTime"]
      [ select
        [ onChangeInt 2000 (OnSelectDateTimeYear c)
        , class "inputDateTime_year"
        ]
        (List.map
          (\y ->
            option
              [ intValue y
              , selected <| Maybe.map Date.year c.input == Just y
              ]
              [ text (toString y ++ "年")
              ]
          )
          (List.unfoldr
            (\b -> if b == 1900
              then Nothing
              else Just (b, b-1)
            )
            2016
          )
        )
      , select
        [ onChangeInt 1 (OnSelectDateTimeMonth c)
        , class "inputDateTime_month"
        ]
        (List.map
          (\m ->
            option
              [ intValue m
              , selected <| Maybe.map Date.monthNumber c.input == Just m
              ]
              [ text (toString m ++ "月")
              ]
          )
          [1..12]
        )
      , select
        [ onChangeInt 1 (OnSelectDateTimeDay c)
        , class "inputDateTime_day"
        ]
        (List.map
          (\d ->
            option
              [ intValue d
              , selected <| Maybe.map Date.day c.input == Just d
              ]
              [ text (toString d ++ "日")
              ]
          )
          [1..31]
        )
      , select
        [ onChangeInt 1 (OnSelectDateTimeHour c)
        , class "inputDateTime_hour"
        ]
        (List.map
          (\d ->
            option
              [ intValue d
              , selected <| Maybe.map Date.hour c.input == Just d
              ]
              [ text (toString d ++ "時")
              ]
          )
          [1..24]
        )
      , select
        [ onChangeInt 1 (OnSelectDateTimeMinute c)
        , class "inputDateTime_minute"
        ]
        (List.map
          (\d ->
            option
              [ intValue d
              , selected <| Maybe.map Date.minute c.input == Just d
              ]
              [ text (toString d ++ "分")
              ]
          )
          [1..59]
        )
      ]
    , div [class "submit"]
      [ button
        [ class "btn default"
        , type' "submit"
        ]
        [ text "送信する"
        ]
      ]
    ]


renderInputRange : InputRangeConfig -> Html Msg
renderInputRange c =
  Html.form
    [ class "submitArea"
    ]
    [ div [class "control-group inputRange"]
      [ div [class "inputRange-labelArea"]
        [ span
          [ style
            [ ( "opacity"
              , toString (toFloat (100 - c.input) / 100)
              )
            , ( "fontWeight"
              , toString ((100 - c.input) * 99 // 1000 * 100)
              )
            ]
          ]
          [ text c.minLabel
          ]
        , span
          [ style
            [ ( "opacity"
              , toString (toFloat c.input / 100)
              )
            , ( "fontWeight"
              , toString (c.input * 99 // 1000 * 100)
              )
            ]
          ]
          [ text c.maxLabel
          ]
        ]
      , input
        [ type' "range"
        , class "inputRange-range"
        , intValue c.input
        , onInputInt 50 (OnInputRange c)
        , onChangeInt 50 (\_ -> OnSubmit (InputRange c))
        ]
        []
      ]
    ]


renderInputPlace : InputPlaceConfig -> Html Msg
renderInputPlace c =
  Html.form
    [ class "submitArea"
    , onSubmit (OnSubmit (InputPlace c))
    ]
    [ div [class "control-group inputPlace"]
      [ input
        [ type' "text"
        , class "inputPlace_region"
        , value c.input.region
        , onInput (OnInputPlaceRegion c)
        ]
        []
      , input
        [ type' "text"
        , class "inputPlace_locality"
        , value c.input.locality
        , onInput (OnInputPlaceLocality c)
        ]
        []
      , input
        [ type' "text"
        , class "inputPlace_streetAddress"
        , value c.input.streetAddress
        , onInput (OnInputPlaceStreetAddress c)
        ]
        []
      , input
        [ type' "text"
        , class "inputPlace_extendedAddress"
        , value c.input.extendedAddress
        , onInput (OnInputPlaceExtendedAddress c)
        ]
        []
      , input
        [ type' "text"
        , class "inputPlace_building"
        , value c.input.building
        , onInput (OnInputPlaceBuilding c)
        ]
        []
      ]
    , div [class "submit"]
      [ button
        [ class "btn default"
        , type' "submit"
        ]
        [ text "送信する"
        ]
      ]
    ]


-- TODO It's just a dummy.
renderInputLocation : Model -> InputLocationConfig -> Html Msg
renderInputLocation model c =
  div [class "popupMap"]
    [ App.map Geocode (Geocode.view model.geocode)
    ]


renderSelectList : SelectListConfig -> Html Msg
renderSelectList c =
  Html.form
    [ class "submitArea"
    , onSubmit (OnSubmit (SelectList c))
    ]
    [ div [class "control-group selectList"]
      [ select
        [ onChange (OnSelectList c)
        , class "form-control"
        ]
        (
          option
            [ value ""
            , disabled True
            ]
            [] ::
          List.map
            (\opt ->
              option
                [ value opt.value
                , selected <| c.input == opt.value
                ]
                [ text opt.label ]
            )
            c.options
        )
      ]
    , div [class "submit"]
      [ button
        [ class "btn default"
        , type' "submit"
        ]
        [ text "送信する"
        ]
      ]
    ]


renderSelectPhoto : SelectPhotoConfig -> Html Msg
renderSelectPhoto c =
  Html.form
    [ class "submitArea"
    , onSubmit (OnSubmit (SelectPhoto c))
    ]
    [ div [class "control-group selectPhoto"]
      [ select
        [ onChange (OnSelectPhoto c)
        , class "form-control"
        ]
        (
          option
            [ value ""
            , disabled True
            ]
            [] ::
          List.map
            (\opt ->
              option
                [ value opt
                ]
                [ text opt ]
            )
            c.options
        )
      ]
    , div [class "submit"]
      [ button
        [ class "btn default"
        , type' "submit"
        ]
        [ text "送信する"
        ]
      ]
    ]


renderMultiSelect : MultiSelectConfig -> Html Msg
renderMultiSelect c =
  let
    left = c.maximum - List.length c.inputs
  in
    Html.form
      [ class "submitArea"
      , onSubmit (OnSubmit (MultiSelect c))
      ]
      [ div [class "control-group multiSelect"]
        <| List.map
          (\select ->
            let
              tagId = "checkbox" ++ escapeSpace select.value
            in
              div [class "checkboxWrapper"]
                [ input
                  [ type' "checkbox"
                  , id  tagId
                  , class "checkboxWrapper_checkbox"
                  , onCheck (OnCheckMultiSelect c select.value)
                  , checked
                    <| List.member select.value (Debug.log "inputs" c.inputs)
                  ]
                  []
                , label
                  [ class "checkboxWrapper_label"
                  , for tagId
                  ]
                  [ text select.label ]
                ]
          )
          c.selection
      , div [class "submit"]
        [ div [class "annotation"]
          [ text "残り: "
          , span
              [ classList
                [ ("error", left < 0)
                ]
              ]
              [ text <| toString left ]
          , text " 個"
          ]
        , button
          [ class "btn default"
          , disabled (left < 0)
          , type' "submit"
          ]
          [ text "送信する"
          ]
        ]
      ]


renderTextArea : TextAreaConfig -> Html Msg
renderTextArea c =
  Html.form
    [ class "submitArea"
    , onSubmit (OnSubmit (TextArea c))
    ]
    [ div [class "control-group textArea"]
      [ textarea
        [ onInput (OnInputTextArea c)
        ]
        [ text c.input
        ]
      ]
    , div [class "submit"]
      [ button
        [ class "btn default"
        , type' "submit"
        ]
        [ text "送信する"
        ]
      ]
    ]


escapeSpace : String -> String
escapeSpace =
  Regex.replace Regex.All (Regex.regex " ") (always "_")



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Sub.map Geocode <| Geocode.subscriptions model.geocode
    ]
