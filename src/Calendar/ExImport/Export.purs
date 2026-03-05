module Calendar.ExImport.Export
  ( module Calendar.ExImport.Model
  , ExportInput
  , ExportFilter
  , ExportFormat(..)
  , component
  , filterItemsForExport
  , exportItemsToCsv
  , exportItemsToIcs
  ) where

import Prelude hiding (div)

import Calendar.ExImport.Model (Item(..), ItemStatus(..), ItemType(..))
import Calendar.Recurrence (RecurrenceRule(..))
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Array (filter, null)
import Data.Date (Date)
import Data.DateTime (DateTime, date)
import Data.Foldable (foldl, foldMap)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', (.~), (^.), lens)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String.CodeUnits as String
import Data.String.Common as StringCommon
import Data.Show.Generic (genericShow)
import Effect.Aff (Aff)
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, get, mkComponent, mkEval, modify_) as H
import Halogen.HTML (button, div, input, option, section, select, text, textarea)
import Halogen.HTML.Events (onClick, onValueChange)
import Halogen.HTML.Properties (placeholder, type_, value)
import Helpers.DateTime as DateTime
import Ui.AgendaRender (renderPanelHeader)
import Ui.Utils (class_)

type ExportInput =
  { items :: Array Item }

data ExportFormat = ExportCSV | ExportICS

derive instance exportFormatEq :: Eq ExportFormat
derive instance exportFormatGeneric :: Generic ExportFormat _
instance exportFormatShow :: Show ExportFormat where
  show = genericShow

type ExportFilter =
  { itemType :: Maybe ItemType
  , status :: Maybe ItemStatus
  , category :: Maybe String
  , startDate :: Maybe Date
  , endDate :: Maybe Date
  }

type ExportState =
  { format :: ExportFormat
  , form :: ExportFormState
  , filter :: ExportFilter
  , output :: String
  }

type ExportFormState =
  { typeFilter :: Maybe ItemType
  , statusFilter :: Maybe ItemStatus
  , categoryFilter :: Maybe String
  , startDate :: Maybe Date
  , endDate :: Maybe Date
  }

exportInitialState :: ExportState
exportInitialState =
  { format: ExportCSV
  , form:
      { typeFilter: Nothing
      , statusFilter: Nothing
      , categoryFilter: Nothing
      , startDate: Nothing
      , endDate: Nothing
      }
  , filter:
      buildFilter
        { typeFilter: Nothing
        , statusFilter: Nothing
        , categoryFilter: Nothing
        , startDate: Nothing
        , endDate: Nothing
        }
  , output: ""
  }

_exportFormat :: Lens' ExportState ExportFormat
_exportFormat =
  lens
    _.format
    (_ { format = _ })

_exportForm :: Lens' ExportState ExportFormState
_exportForm =
  lens
    _.form
    (_ { form = _ })

_exportFilter :: Lens' ExportState ExportFilter
_exportFilter =
  lens
    _.filter
    (_ { filter = _ })

_exportOutput :: Lens' ExportState String
_exportOutput =
  lens
    _.output
    (_ { output = _ })

data Action
  = Receive ExportInput
  | FormatChanged String
  | TypeFilterChanged String
  | StatusFilterChanged String
  | CategoryFilterChanged String
  | StartDateChanged String
  | EndDateChanged String
  | Generate
  | ClearOutput

component :: forall q. H.Component q ExportInput Void Aff
component =
  H.mkComponent
    { initialState: \input -> { export: exportInitialState, items: input.items }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, receive = Just <<< Receive }
    }

handleAction :: Action -> H.HalogenM { export :: ExportState, items :: Array Item } Action () Void Aff Unit
handleAction = case _ of
  Receive input -> H.modify_ (_ { items = input.items })
  FormatChanged raw -> H.modify_ $ updateExport (\st -> st # _exportFormat .~ parseExportFormat raw)
  TypeFilterChanged raw -> H.modify_ $ updateForm (_ { typeFilter = parseTypeFilter raw })
  StatusFilterChanged raw -> H.modify_ $ updateForm (_ { statusFilter = parseStatusFilter raw })
  CategoryFilterChanged raw -> H.modify_ $ updateForm (_ { categoryFilter = toOptionalString raw })
  StartDateChanged raw -> H.modify_ $ updateForm (_ { startDate = parseDateInput raw })
  EndDateChanged raw -> H.modify_ $ updateForm (_ { endDate = parseDateInput raw })
  Generate -> do
    st <- H.get
    let exportState = st.export
    let
      filtered = filterItemsForExport (exportState ^. _exportFilter) st.items
      output =
        case exportState ^. _exportFormat of
          ExportICS -> exportItemsToIcs filtered
          ExportCSV -> exportItemsToCsv filtered
    H.modify_ (_ { export = exportState # _exportOutput .~ output })
  ClearOutput -> H.modify_ (updateExport $ _exportOutput .~ "")
  where
  updateExport f st = st { export = f st.export }
  updateForm f st =
    let
      exportState = st.export
      nextForm = f (exportState ^. _exportForm)
      nextFilter = buildFilter nextForm
    in
      st { export = (exportState # _exportForm .~ nextForm) # _exportFilter .~ nextFilter }

render :: forall m. { export :: ExportState, items :: Array Item } -> H.ComponentHTML Action () m
render
  { export:
      { format
      , form
      , output
      }
  } =
  section [ class_ "calendar-export" ]
    [ renderPanelHeader
        { baseClass: "calendar-export"
        , title: "Export"
        , subtitle: "Filtres: type, catégorie, statut, période."
        }
        []
    , div [ class_ "calendar-export-controls" ]
        [ div [ class_ "calendar-export-control" ]
            [ div [ class_ "calendar-notifications-label" ] [ text "Format" ]
            , select
                [ class_ "form-select calendar-sort-select"
                , onValueChange FormatChanged
                , value (exportFormatValue format)
                ]
                [ option [ value "csv" ] [ text "CSV" ]
                , option [ value "ics" ] [ text "ICS" ]
                ]
            ]
        , div [ class_ "calendar-export-control" ]
            [ div [ class_ "calendar-notifications-label" ] [ text "Type" ]
            , select
                [ class_ "form-select calendar-sort-select"
                , onValueChange TypeFilterChanged
                , value (typeFilterValue form.typeFilter)
                ]
                [ option [ value "" ] [ text "Tous" ]
                , option [ value "INTENTION" ] [ text "Intention" ]
                , option [ value "BLOC_PLANIFIE" ] [ text "Bloc planifié" ]
                ]
            ]
        , div [ class_ "calendar-export-control" ]
            [ div [ class_ "calendar-notifications-label" ] [ text "Statut" ]
            , select
                [ class_ "form-select calendar-sort-select"
                , onValueChange StatusFilterChanged
                , value (statusFilterValue form.statusFilter)
                ]
                [ option [ value "" ] [ text "Tous" ]
                , option [ value "TODO" ] [ text "TODO" ]
                , option [ value "IN_PROGRESS" ] [ text "IN_PROGRESS" ]
                , option [ value "DONE" ] [ text "DONE" ]
                , option [ value "CANCELED" ] [ text "CANCELED" ]
                ]
            ]
        , div [ class_ "calendar-export-control" ]
            [ div [ class_ "calendar-notifications-label" ] [ text "Catégorie" ]
            , input
                [ class_ "form-control calendar-input"
                , placeholder "Ex: Sport"
                , value (categoryFilterValue form.categoryFilter)
                , onValueChange CategoryFilterChanged
                ]
            ]
        , div [ class_ "calendar-export-control" ]
            [ div [ class_ "calendar-notifications-label" ] [ text "Début" ]
            , input
                [ class_ "form-control calendar-input"
                , type_ InputDate
                , value (dateValue form.startDate)
                , onValueChange StartDateChanged
                ]
            ]
        , div [ class_ "calendar-export-control" ]
            [ div [ class_ "calendar-notifications-label" ] [ text "Fin" ]
            , input
                [ class_ "form-control calendar-input"
                , type_ InputDate
                , value (dateValue form.endDate)
                , onValueChange EndDateChanged
                ]
            ]
        ]
    , div [ class_ "calendar-export-actions" ]
        [ button [ class_ "btn btn-sm btn-primary", onClick (const Generate) ] [ text "Générer" ]
        , button [ class_ "btn btn-sm btn-outline-secondary", onClick (const ClearOutput) ] [ text "Effacer" ]
        ]
    , if output == "" then text ""
      else
        textarea
          [ class_ "form-control calendar-export-textarea"
          , value output
          ]
    ]

exportFormatValue :: ExportFormat -> String
exportFormatValue ExportCSV = "csv"
exportFormatValue ExportICS = "ics"

parseExportFormat :: String -> ExportFormat
parseExportFormat raw =
  if raw == "ics" then ExportICS else ExportCSV

buildFilter :: ExportFormState -> ExportFilter
buildFilter form =
  { itemType: form.typeFilter
  , status: form.statusFilter
  , category: form.categoryFilter
  , startDate: form.startDate
  , endDate: form.endDate
  }

filterItemsForExport :: ExportFilter -> Array Item -> Array Item
filterItemsForExport criteria items = filter (matchesFilter criteria) items

matchesFilter :: { itemType :: Maybe ItemType, status :: Maybe ItemStatus, category :: Maybe String, startDate :: Maybe Date, endDate :: Maybe Date } -> Item -> Boolean
matchesFilter criteria (Item item) =
  matchesType && matchesStatus && matchesCategory && matchesStart && matchesEnd
  where
  matchesType = maybe true (\target -> item.itemType == target) criteria.itemType
  matchesStatus = maybe true (\target -> item.status == target) criteria.status
  matchesCategory = criteria.category # maybe true \target ->
    item.category # maybe false \categoryValue -> normalizeHeader categoryValue == normalizeHeader target
  dateKey = date item.windowStart
  matchesStart = criteria.startDate # maybe true \start -> dateKey >= start
  matchesEnd = criteria.endDate # maybe true \end -> dateKey <= end

normalizeHeader :: String -> String
normalizeHeader = StringCommon.trim >>> StringCommon.toLower

exportItemsToCsv :: Array Item -> String
exportItemsToCsv items =
  StringCommon.joinWith "\n" ([ header ] <> rows)
  where
  header =
    "type,titre,fenetre_debut,fenetre_fin,statut,categorie,source_item_id,actual_duration_minutes,recurrence_rule_type,recurrence_rule_interval_days,recurrence_exception_dates"
  rows = map exportCsvRow items

exportCsvRow :: Item -> String
exportCsvRow (Item item) =
  let
    category = fromMaybe "" item.category
    sourceItemId = foldMap identity item.sourceItemId
    actualDuration = foldMap show item.actualDurationMinutes
    recurrenceRuleType = recurrenceRuleTypeValue item.recurrenceRule
    recurrenceRuleInterval = recurrenceRuleIntervalValue item.recurrenceRule
    recurrenceExceptions = StringCommon.joinWith ";" (map DateTime.formatLocalDate item.recurrenceExceptionDates)
  in
    StringCommon.joinWith "," $
      map csvEscape
        [ exportItemType item.itemType
        , item.title
        , DateTime.formatLocalDateTime item.windowStart
        , DateTime.formatLocalDateTime item.windowEnd
        , exportItemStatus item.status
        , category
        , sourceItemId
        , actualDuration
        , recurrenceRuleType
        , recurrenceRuleInterval
        , recurrenceExceptions
        ]

csvEscape :: String -> String
csvEscape value = "\"" <> escapeQuotes value <> "\""

exportItemsToIcs :: Array Item -> String
exportItemsToIcs items =
  let
    header = [ "BEGIN:VCALENDAR", "VERSION:2.0", "PRODID:-//FAVS//EN" ]
    events = items >>= exportIcsEvent
  in
    StringCommon.joinWith "\n" (header <> events <> [ "END:VCALENDAR" ])

exportIcsEvent :: Item -> Array String
exportIcsEvent (Item item) =
  let
    start = toIcsDateTime item.windowStart
    end = toIcsDateTime item.windowEnd
    categoryLine = foldMap (\value -> [ "CATEGORIES:" <> value ]) item.category
    recurrenceLine = foldMap (\rule -> [ "RRULE:" <> toIcsRrule rule ]) item.recurrenceRule
    exceptionLine =
      if null item.recurrenceExceptionDates then []
      else [ "EXDATE:" <> StringCommon.joinWith "," (map toIcsDate item.recurrenceExceptionDates) ]
    sourceItemId = foldMap identity item.sourceItemId
    actualDuration = foldMap show item.actualDurationMinutes
  in
    [ "BEGIN:VEVENT"
    , "SUMMARY:" <> item.title
    , "DTSTART:" <> start
    , "DTEND:" <> end
    , "X-FAVS-TYPE:" <> exportItemType item.itemType
    , "X-FAVS-STATUS:" <> exportItemStatus item.status
    , "X-FAVS-SOURCE-ITEM-ID:" <> sourceItemId
    , "X-FAVS-ACTUAL-DURATION-MINUTES:" <> actualDuration
    ]
      <> categoryLine
      <> recurrenceLine
      <> exceptionLine
      <> [ "END:VEVENT" ]

toIcsDateTime :: DateTime -> String
toIcsDateTime dateTime =
  let
    formatted = DateTime.formatLocalDateTime dateTime
  in
    if String.length formatted >= 16 then
      String.slice 0 4 formatted
        <> String.slice 5 7 formatted
        <> String.slice 8 10 formatted
        <> "T"
        <> String.slice 11 13 formatted
        <>
          String.slice 14 16 formatted
    else formatted

toIcsDate :: Date -> String
toIcsDate dateValue' =
  let
    formatted = DateTime.formatLocalDate dateValue'
  in
    if String.length formatted >= 10 then
      String.slice 0 4 formatted <> String.slice 5 7 formatted <> String.slice 8 10 formatted
    else formatted

toIcsRrule :: RecurrenceRule -> String
toIcsRrule Daily = "FREQ=DAILY"
toIcsRrule Weekly = "FREQ=WEEKLY"
toIcsRrule Monthly = "FREQ=MONTHLY"
toIcsRrule Yearly = "FREQ=YEARLY"
toIcsRrule (EveryXDays interval) = "FREQ=DAILY;INTERVAL=" <> show interval

exportItemType :: ItemType -> String
exportItemType Intention = "INTENTION"
exportItemType ScheduledBlock = "BLOC_PLANIFIE"

exportItemStatus :: ItemStatus -> String
exportItemStatus Todo = "TODO"
exportItemStatus InProgress = "IN_PROGRESS"
exportItemStatus Done = "DONE"
exportItemStatus Canceled = "CANCELED"

toOptionalString :: String -> Maybe String
toOptionalString raw = if trimmed == "" then Nothing else Just trimmed
  where
  trimmed = StringCommon.trim raw

parseTypeFilter :: String -> Maybe ItemType
parseTypeFilter "INTENTION" = Just Intention
parseTypeFilter "BLOC_PLANIFIE" = Just ScheduledBlock
parseTypeFilter _ = Nothing

parseStatusFilter :: String -> Maybe ItemStatus
parseStatusFilter "TODO" = Just Todo
parseStatusFilter "IN_PROGRESS" = Just InProgress
parseStatusFilter "DONE" = Just Done
parseStatusFilter "CANCELED" = Just Canceled
parseStatusFilter _ = Nothing

parseDateInput :: String -> Maybe Date
parseDateInput raw = do
  trimmed <- toOptionalString raw
  DateTime.parseLocalDate trimmed

typeFilterValue :: Maybe ItemType -> String
typeFilterValue Nothing = ""
typeFilterValue (Just Intention) = "INTENTION"
typeFilterValue (Just ScheduledBlock) = "BLOC_PLANIFIE"

statusFilterValue :: Maybe ItemStatus -> String
statusFilterValue Nothing = ""
statusFilterValue (Just Todo) = "TODO"
statusFilterValue (Just InProgress) = "IN_PROGRESS"
statusFilterValue (Just Done) = "DONE"
statusFilterValue (Just Canceled) = "CANCELED"

categoryFilterValue :: Maybe String -> String
categoryFilterValue = foldMap identity

dateValue :: Maybe Date -> String
dateValue = foldMap DateTime.formatLocalDate

recurrenceRuleTypeValue :: Maybe RecurrenceRule -> String
recurrenceRuleTypeValue Nothing = ""
recurrenceRuleTypeValue (Just rule) =
  case rule of
    Daily -> "daily"
    Weekly -> "weekly"
    Monthly -> "monthly"
    Yearly -> "yearly"
    EveryXDays _ -> "every"

recurrenceRuleIntervalValue :: Maybe RecurrenceRule -> String
recurrenceRuleIntervalValue Nothing = ""
recurrenceRuleIntervalValue (Just (EveryXDays interval)) = show interval
recurrenceRuleIntervalValue _ = ""

escapeQuotes :: String -> String
escapeQuotes raw =
  foldl
    (\acc ch -> if ch == '"' then acc <> "\"\"" else acc <> String.singleton ch)
    ""
    (String.toCharArray raw)
