module Calendar.Recurrence
  ( RecurrenceRule(..)
  , RecurrenceDraft
  , defaultRecurrenceDraft
  , draftFromRecurrence
  , draftToRecurrence
  , generateOccurrencesForMonth
  , RecurrenceCommand(..)
  , component
  ) where

import Prelude hiding (div)

import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Argonaut.Encode (class EncodeJson, (:=), (~>))
import Data.Array (delete, elem, filter)
import Data.Date (Date, canonicalDate, exactDate, month, year, day)
import Data.DateTime (DateTime(..), adjust, date)
import Data.Either (Either(..))
import Data.Enum (fromEnum, toEnum)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits as String
import Data.String.Common as StringCommon
import Data.Time (Time(..))
import Data.Time.Duration (Days(..))
import Effect.Aff (Aff)
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, get, mkComponent, mkEval, modify_, raise) as H
import Halogen.HTML (button, div, input, option, select, text, ul, li)
import Halogen.HTML.Core (AttrName(..))
import Halogen.HTML.Events (onClick, onValueChange)
import Halogen.HTML.Properties (attr, placeholder, type_, value)
import Ui.Utils (class_)


data RecurrenceRule
  = Daily
  | Weekly
  | Monthly
  | Yearly
  | EveryXDays Int

derive instance recurrenceRuleGeneric :: Generic RecurrenceRule _
derive instance recurrenceRuleEq :: Eq RecurrenceRule

instance recurrenceRuleShow :: Show RecurrenceRule where
  show = genericShow

instance recurrenceRuleEncodeJson :: EncodeJson RecurrenceRule where
  encodeJson rule =
    case rule of
      Daily ->
        "type" := "DAILY" ~> jsonEmptyObject
      Weekly ->
        "type" := "WEEKLY" ~> jsonEmptyObject
      Monthly ->
        "type" := "MONTHLY" ~> jsonEmptyObject
      Yearly ->
        "type" := "YEARLY" ~> jsonEmptyObject
      EveryXDays interval ->
        "type" := "EVERY_X_DAYS"
          ~> "interval_days" := interval
          ~> jsonEmptyObject

instance recurrenceRuleDecodeJson :: DecodeJson RecurrenceRule where
  decodeJson json = do
    obj <- decodeJson json
    kind <- obj .: "type"
    case kind of
      "DAILY" -> pure Daily
      "WEEKLY" -> pure Weekly
      "MONTHLY" -> pure Monthly
      "YEARLY" -> pure Yearly
      "EVERY_X_DAYS" -> EveryXDays <$> obj .: "interval_days"
      _ -> Left $ UnexpectedValue json


type RecurrenceDraft =
  { ruleType :: String
  , intervalRaw :: String
  , exceptionInput :: String
  , exceptions :: Array String
  }

defaultRecurrenceDraft :: RecurrenceDraft
defaultRecurrenceDraft =
  { ruleType: "none"
  , intervalRaw: ""
  , exceptionInput: ""
  , exceptions: []
  }

draftFromRecurrence :: Maybe RecurrenceRule -> Array String -> RecurrenceDraft
draftFromRecurrence rule exceptions =
  case rule of
    Nothing ->
      defaultRecurrenceDraft { exceptions = exceptions }
    Just Daily ->
      defaultRecurrenceDraft { ruleType = "daily", exceptions = exceptions }
    Just Weekly ->
      defaultRecurrenceDraft { ruleType = "weekly", exceptions = exceptions }
    Just Monthly ->
      defaultRecurrenceDraft { ruleType = "monthly", exceptions = exceptions }
    Just Yearly ->
      defaultRecurrenceDraft { ruleType = "yearly", exceptions = exceptions }
    Just (EveryXDays interval) ->
      defaultRecurrenceDraft
        { ruleType = "every"
        , intervalRaw = show interval
        , exceptions = exceptions
        }

draftToRecurrence :: RecurrenceDraft -> Either String { rule :: Maybe RecurrenceRule, exceptions :: Array String }
draftToRecurrence draft =
  let
    exceptions = draft.exceptions
    cleanedExceptions = filterValidExceptions exceptions
    invalid = invalidExceptions exceptions
  in
    if invalid /= [] then
      Left "Exceptions invalides (format attendu YYYY-MM-DD)."
    else
      case draft.ruleType of
        "none" -> Right { rule: Nothing, exceptions: [] }
        "daily" -> Right { rule: Just Daily, exceptions: cleanedExceptions }
        "weekly" -> Right { rule: Just Weekly, exceptions: cleanedExceptions }
        "monthly" -> Right { rule: Just Monthly, exceptions: cleanedExceptions }
        "yearly" -> Right { rule: Just Yearly, exceptions: cleanedExceptions }
        "every" ->
          case parsePositiveInt draft.intervalRaw of
            Nothing -> Left "Intervalle invalide (jours)."
            Just interval -> Right { rule: Just (EveryXDays interval), exceptions: cleanedExceptions }
        _ -> Right { rule: Nothing, exceptions: [] }
  where
  filterValidExceptions = filter (\raw -> isValidDate raw)
  invalidExceptions = filter (\raw -> not (isValidDate raw))
  isValidDate raw = case parseDateLocal raw of
    Just _ -> true
    Nothing -> false

generateOccurrencesForMonth :: RecurrenceRule -> Array String -> String -> Array String
generateOccurrencesForMonth rule exceptions start =
  case parseDateTimeLocal start of
    Nothing -> []
    Just startDt ->
      let
        targetMonth = month (date startDt)
        targetYear = year (date startDt)
        sameMonth dt = month (date dt) == targetMonth && year (date dt) == targetYear

        collectOccurrences current acc =
          if not (sameMonth current) then acc
          else case nextOccurrence rule current of
            Nothing -> acc <> [ current ]
            Just next -> collectOccurrences next (acc <> [ current ])

        occurrences = collectOccurrences startDt []
      in
        occurrences
          # map formatDate
          # filter (\dateStr -> not (elem dateStr exceptions))

nextOccurrence :: RecurrenceRule -> DateTime -> Maybe DateTime
nextOccurrence rule dt =
  case rule of
    Daily -> addDays 1 dt
    Weekly -> addDays 7 dt
    EveryXDays interval -> addDays interval dt
    Monthly -> Just (addMonths 1 dt)
    Yearly -> Just (addMonths 12 dt)

parseDateTimeLocal :: String -> Maybe DateTime
parseDateTimeLocal raw = do
  yearNum <- parseInt (slice 0 4)
  monthNum <- parseInt (slice 5 7)
  dayNum <- parseInt (slice 8 10)
  hourNum <- parseInt (slice 11 13)
  minuteNum <- parseInt (slice 14 16)
  month' <- toEnum monthNum
  day' <- toEnum dayNum
  hour' <- toEnum hourNum
  minute' <- toEnum minuteNum
  year' <- toEnum yearNum
  date' <- exactDate year' month' day'
  second <- toEnum 0
  millisecond <- toEnum 0
  pure $ DateTime date' (Time hour' minute' second millisecond)
  where
  slice start end = String.slice start end raw
  parseInt str = Int.fromString str

parseDateLocal :: String -> Maybe Date
parseDateLocal raw = do
  yearNum <- parseInt (String.slice 0 4 raw)
  monthNum <- parseInt (String.slice 5 7 raw)
  dayNum <- parseInt (String.slice 8 10 raw)
  month' <- toEnum monthNum
  day' <- toEnum dayNum
  year' <- toEnum yearNum
  exactDate year' month' day'
  where
  parseInt str = Int.fromString str

parsePositiveInt :: String -> Maybe Int
parsePositiveInt raw =
  Int.fromString (StringCommon.trim raw) >>= \val ->
    if val > 0 then Just val else Nothing

addDays :: Int -> DateTime -> Maybe DateTime
addDays n dt = adjust (Days (Int.toNumber n)) dt

addMonths :: Int -> DateTime -> DateTime
addMonths n (DateTime d t) =
  let
    y = fromEnum (year d)
    m = fromEnum (month d)
    dNum = fromEnum (day d)
    total = (m - 1) + n
    newYear = y + Int.quot total 12
    newMonth = (Int.rem total 12) + 1
    newDate =
      case { year: toEnum newYear, month: toEnum newMonth, day: toEnum dNum } of
        { year: Just y', month: Just m', day: Just d' } -> canonicalDate y' m' d'
        _ -> d
  in
    DateTime newDate t

formatDate :: DateTime -> String
formatDate dt =
  let
    y = Int.toStringAs Int.decimal (fromEnum (year (date dt)))
    m = pad2 (fromEnum (month (date dt)))
    d = pad2 (fromEnum (day (date dt)))
  in
    y <> "-" <> m <> "-" <> d

pad2 :: Int -> String
pad2 n =
  let
    raw = Int.toStringAs Int.decimal n
  in
    if String.length raw == 1 then "0" <> raw else raw


newtype RecurrenceCommand = RecurrenceApplied RecurrenceDraft
type State = RecurrenceDraft
data Action
  = Receive RecurrenceDraft
  | RecurrenceRuleChanged String
  | RecurrenceIntervalChanged String
  | RecurrenceExceptionInputChanged String
  | RecurrenceAddException
  | RecurrenceRemoveException String
  | Apply

component :: forall q. H.Component q RecurrenceDraft RecurrenceCommand Aff
component =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }

handleAction :: Action -> H.HalogenM State Action () RecurrenceCommand Aff Unit
handleAction = case _ of
  Receive draft -> H.modify_ (const draft)
  RecurrenceRuleChanged raw -> H.modify_ _ { ruleType = raw }
  RecurrenceIntervalChanged raw -> H.modify_ _ { intervalRaw = raw }
  RecurrenceExceptionInputChanged raw -> H.modify_ _ { exceptionInput = raw }
  RecurrenceAddException -> H.modify_ \draft ->
    let
      cleaned = StringCommon.trim draft.exceptionInput
      nextExceptions =
        if cleaned == "" || elem cleaned draft.exceptions then draft.exceptions
        else draft.exceptions <> [ cleaned ]
    in
      draft { exceptions = nextExceptions, exceptionInput = "" }
  RecurrenceRemoveException dateStr -> H.modify_ \draft -> draft { exceptions = delete dateStr draft.exceptions }
  Apply -> do
    draft <- H.get
    H.raise (RecurrenceApplied draft)

render :: forall m. State -> H.ComponentHTML Action () m
render draft =
  div [ class_ "calendar-recurrence" ]
    [ div [ class_ "calendar-modal-field" ]
        [ div [ class_ "calendar-notifications-label" ] [ text "Récurrence" ]
        , select
            [ class_ "form-select calendar-input"
            , onValueChange RecurrenceRuleChanged
            , value draft.ruleType
            ]
            [ option [ value "none" ] [ text "Aucune" ]
            , option [ value "daily" ] [ text "Quotidienne" ]
            , option [ value "weekly" ] [ text "Hebdomadaire" ]
            , option [ value "monthly" ] [ text "Mensuelle" ]
            , option [ value "yearly" ] [ text "Annuelle" ]
            , option [ value "every" ] [ text "Tous les X jours" ]
            ]
        ]
    , if draft.ruleType /= "every" then text ""
      else
        div [ class_ "calendar-modal-field" ]
          [ div [ class_ "calendar-notifications-label" ] [ text "Intervalle (jours)" ]
          , input
              [ class_ "form-control calendar-input"
              , type_ InputNumber
              , placeholder "Ex: 3"
              , onValueChange RecurrenceIntervalChanged
              , value draft.intervalRaw
              ]
          ]
    , div [ class_ "calendar-modal-field" ]
        [ div [ class_ "calendar-notifications-label" ] [ text "Exceptions (dates à ignorer)" ]
        , div [ class_ "calendar-recurrence-exception-row" ]
            [ input
                [ class_ "form-control calendar-input"
                , type_ InputDate
                , attr (AttrName "lang") "fr"
                , placeholder "YYYY-MM-DD"
                , onValueChange RecurrenceExceptionInputChanged
                , value draft.exceptionInput
                ]
            , button
                [ class_ "btn btn-sm btn-outline-secondary calendar-recurrence-add"
                , onClick $ const RecurrenceAddException
                ]
                [ text "Ajouter" ]
            ]
        , if draft.exceptions == [] then text ""
          else ul [ class_ "calendar-recurrence-exceptions" ]
            (map renderException draft.exceptions)
        ]
    , button
        [ class_ "btn btn-sm btn-outline-primary calendar-recurrence-apply"
        , onClick (const Apply)
        ]
        [ text "Appliquer" ]
    ]
  where
  renderException dateStr =
    li [ class_ "calendar-recurrence-exception" ]
      [ div [ class_ "calendar-recurrence-exception-date" ] [ text dateStr ]
      , button
          [ class_ "btn btn-sm btn-outline-secondary calendar-recurrence-remove"
          , onClick (const $ RecurrenceRemoveException dateStr)
          ]
          [ text "Supprimer" ]
      ]
