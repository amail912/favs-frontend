module Calendar.RecurrenceEditor
  ( RecurrenceDraft
  , RecurrenceAction(..)
  , defaultRecurrenceDraft
  , draftFromRecurrence
  , draftToRecurrence
  , applyRecurrenceAction
  , renderRecurrenceEditor
  ) where

import Prelude hiding (div)

import Calendar.Helpers (parseDateLocal, parsePositiveInt)
import Calendar.Model (RecurrenceRule(..))
import Data.Array (delete, elem, filter)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String.Common as StringCommon
import Halogen.HTML (HTML, button, div, input, option, select, text, ul, li)
import Halogen.HTML.Core (AttrName(..))
import Halogen.HTML.Events (onClick, onValueChange)
import Halogen.HTML.Properties (attr, placeholder, type_, value)
import DOM.HTML.Indexed.InputType (InputType(..))
import Ui.Utils (class_)

type RecurrenceDraft =
  { ruleType :: String
  , intervalRaw :: String
  , exceptions :: Array String
  , exceptionInput :: String
  }

defaultRecurrenceDraft :: RecurrenceDraft
defaultRecurrenceDraft =
  { ruleType: "none"
  , intervalRaw: ""
  , exceptions: []
  , exceptionInput: ""
  }

data RecurrenceAction
  = RecurrenceRuleChanged String
  | RecurrenceIntervalChanged String
  | RecurrenceExceptionInputChanged String
  | RecurrenceAddException
  | RecurrenceRemoveException String

draftFromRecurrence :: Maybe RecurrenceRule -> Array String -> RecurrenceDraft
draftFromRecurrence rule exceptions =
  case rule of
    Nothing ->
      defaultRecurrenceDraft { exceptions = exceptions }
    Just RecurrenceDaily ->
      defaultRecurrenceDraft { ruleType = "daily", exceptions = exceptions }
    Just RecurrenceWeekly ->
      defaultRecurrenceDraft { ruleType = "weekly", exceptions = exceptions }
    Just RecurrenceMonthly ->
      defaultRecurrenceDraft { ruleType = "monthly", exceptions = exceptions }
    Just RecurrenceYearly ->
      defaultRecurrenceDraft { ruleType = "yearly", exceptions = exceptions }
    Just (RecurrenceEveryXDays interval) ->
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
        "daily" -> Right { rule: Just RecurrenceDaily, exceptions: cleanedExceptions }
        "weekly" -> Right { rule: Just RecurrenceWeekly, exceptions: cleanedExceptions }
        "monthly" -> Right { rule: Just RecurrenceMonthly, exceptions: cleanedExceptions }
        "yearly" -> Right { rule: Just RecurrenceYearly, exceptions: cleanedExceptions }
        "every" ->
          case parsePositiveInt draft.intervalRaw of
            Nothing -> Left "Intervalle invalide (jours)."
            Just interval -> Right { rule: Just (RecurrenceEveryXDays interval), exceptions: cleanedExceptions }
        _ -> Right { rule: Nothing, exceptions: [] }
  where
  filterValidExceptions = filter (\raw -> isValidDate raw)
  invalidExceptions = filter (\raw -> not (isValidDate raw))
  isValidDate raw = case parseDateLocal raw of
    Just _ -> true
    Nothing -> false

applyRecurrenceAction :: RecurrenceAction -> RecurrenceDraft -> RecurrenceDraft
applyRecurrenceAction action draft =
  case action of
    RecurrenceRuleChanged raw ->
      draft { ruleType = raw }
    RecurrenceIntervalChanged raw ->
      draft { intervalRaw = raw }
    RecurrenceExceptionInputChanged raw ->
      draft { exceptionInput = raw }
    RecurrenceAddException ->
      let
        cleaned = StringCommon.trim draft.exceptionInput
        nextExceptions =
          if cleaned == "" || elem cleaned draft.exceptions then draft.exceptions
          else draft.exceptions <> [ cleaned ]
      in
        draft { exceptions = nextExceptions, exceptionInput = "" }
    RecurrenceRemoveException dateStr ->
      draft { exceptions = delete dateStr draft.exceptions }

renderRecurrenceEditor :: forall w. RecurrenceDraft -> HTML w RecurrenceAction
renderRecurrenceEditor draft =
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
    , if draft.ruleType /= "every" then text "" else
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
                , onClick (const RecurrenceAddException)
                ]
                [ text "Ajouter" ]
            ]
        , if draft.exceptions == [] then text ""
          else ul [ class_ "calendar-recurrence-exceptions" ]
            (map renderException draft.exceptions)
        ]
    ]
  where
  renderException dateStr =
    li [ class_ "calendar-recurrence-exception" ]
      [ div [ class_ "calendar-recurrence-exception-date" ] [ text dateStr ]
      , button
          [ class_ "btn btn-sm btn-outline-secondary calendar-recurrence-remove"
          , onClick (const (RecurrenceRemoveException dateStr))
          ]
          [ text "Supprimer" ]
      ]
