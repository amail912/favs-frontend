module Ui.DateTimePicker
  ( Input
  , splitIsoLocalDateTime
  , combineIsoLocalDateTimeParts
  , component
  ) where

import Prelude hiding (div)

import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as String
import Effect.Aff (Aff)
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, get, mkComponent, mkEval, modify_, raise) as H
import Halogen.HTML (div, input)
import Halogen.HTML.Core (AttrName(..))
import Halogen.HTML.Events (onValueChange)
import Halogen.HTML.Properties (attr, placeholder, type_, value)
import Helpers.DateTime as DateTime
import Ui.Utils (class_)

type Input =
  { label :: String
  , value :: String
  , isMobile :: Boolean
  }

type State =
  { inputValue :: String
  , label :: String
  , isMobile :: Boolean
  , dateValue :: String
  , timeValue :: String
  }

data Action
  = Receive Input
  | DateChanged String
  | TimeChanged String
  | NativeChanged String

splitIsoLocalDateTime :: String -> { dateValue :: String, timeValue :: String }
splitIsoLocalDateTime raw =
  if DateTime.isLocalDateTime raw then
    { dateValue: String.take 10 raw
    , timeValue: String.drop 11 raw
    }
  else
    { dateValue: "", timeValue: "" }

combineIsoLocalDateTimeParts :: String -> String -> Maybe String
combineIsoLocalDateTimeParts dateValue timeValue =
  let
    combined = dateValue <> "T" <> timeValue
  in
    if DateTime.isLocalDate dateValue && DateTime.isLocalTime timeValue && DateTime.isLocalDateTime combined then
      Just combined
    else
      Nothing

component :: forall q. H.Component q Input String Aff
component =
  H.mkComponent
    { initialState: stateFromInput
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }

handleAction :: Action -> H.HalogenM State Action () String Aff Unit
handleAction = case _ of
  Receive input -> do
    state <- H.get
    if input.value == state.inputValue && input.isMobile == state.isMobile && input.label == state.label then
      pure unit
    else
      H.modify_ (const (stateFromInput input))
  DateChanged nextDate -> do
    H.modify_ _ { dateValue = nextDate }
    emitCombinedValue
  TimeChanged nextTime -> do
    H.modify_ _ { timeValue = nextTime }
    emitCombinedValue
  NativeChanged raw -> do
    let
      { dateValue, timeValue } = splitIsoLocalDateTime raw
    H.modify_
      _
        { inputValue = raw
        , dateValue = dateValue
        , timeValue = timeValue
        }
    H.raise raw
  where
  emitCombinedValue = do
    state <- H.get
    case combineIsoLocalDateTimeParts state.dateValue state.timeValue of
      Just combined -> do
        H.modify_ _ { inputValue = combined }
        H.raise combined
      Nothing ->
        pure unit

stateFromInput :: Input -> State
stateFromInput { label, value: inputValue, isMobile } =
  let
    { dateValue, timeValue } = splitIsoLocalDateTime inputValue
  in
    { inputValue
    , label
    , isMobile
    , dateValue
    , timeValue
    }

render :: forall m. State -> H.ComponentHTML Action () m
render { inputValue, label, isMobile, dateValue, timeValue } =
  let
    wrapperClass =
      if isMobile then
        "ui-datetime-picker ui-datetime-picker--native"
      else
        "ui-datetime-picker ui-datetime-picker--desktop"
  in
    div
      [ class_ wrapperClass
      , attr (AttrName "data-datetime-label") label
      ]
      if isMobile then
        [ input
            [ class_ "form-control calendar-input ui-datetime-picker__native"
            , type_ InputDatetimeLocal
            , attr (AttrName "lang") "fr"
            , placeholder label
            , onValueChange NativeChanged
            , value inputValue
            ]
        ]
      else
        [ input
            [ class_ "form-control calendar-input ui-datetime-picker__date"
            , type_ InputDate
            , attr (AttrName "lang") "fr"
            , attr (AttrName "aria-label") (label <> " date")
            , onValueChange DateChanged
            , value dateValue
            ]
        , input
            [ class_ "form-control calendar-input ui-datetime-picker__time"
            , type_ InputTime
            , attr (AttrName "lang") "fr"
            , attr (AttrName "step") "60"
            , attr (AttrName "aria-label") (label <> " heure")
            , onValueChange TimeChanged
            , value timeValue
            ]
        ]
