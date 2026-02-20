module Agenda.Exports
  ( exportFormatValue
  , parseExportFormat
  , parseExportItemType
  , parseExportStatus
  , filterItemsForExport
  , exportItemsToCsv
  , exportItemsToIcs
  ) where

import Prelude

import Agenda.Model (CalendarItem(..), CalendarItemContent, ExportFilter, ExportFormat(..), ItemStatus(..), ItemType(..))
import Data.Array (filter)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String.CodeUnits as String
import Data.String.Common as StringCommon

exportFormatValue :: ExportFormat -> String
exportFormatValue ExportCSV = "csv"
exportFormatValue ExportICS = "ics"

parseExportFormat :: String -> ExportFormat
parseExportFormat raw =
  if raw == "ics" then ExportICS else ExportCSV

parseExportItemType :: String -> Maybe ItemType
parseExportItemType raw =
  case raw of
    "INTENTION" -> Just Intention
    "BLOC_PLANIFIE" -> Just ScheduledBlock
    _ -> Nothing

parseExportStatus :: String -> Maybe ItemStatus
parseExportStatus raw =
  case raw of
    "TODO" -> Just Todo
    "EN_COURS" -> Just EnCours
    "FAIT" -> Just Fait
    "ANNULE" -> Just Annule
    _ -> Nothing

filterItemsForExport :: ExportFilter -> Array CalendarItem -> Array CalendarItem
filterItemsForExport criteria items =
  filter (matchesFilter criteria) items

matchesFilter :: ExportFilter -> CalendarItem -> Boolean
matchesFilter criteria item =
  let
    content = calendarItemContent item
    matchesType = maybe true (\target -> content.itemType == target) criteria.itemType
    matchesStatus = maybe true (\target -> content.status == target) criteria.status
    matchesCategory =
      case criteria.category of
        Nothing -> true
        Just target ->
          case content.category of
            Nothing -> false
            Just value -> normalizeHeader value == normalizeHeader target
    dateKey = datePart content.windowStart
    matchesStart =
      case criteria.startDate of
        Nothing -> true
        Just start -> dateKey >= start
    matchesEnd =
      case criteria.endDate of
        Nothing -> true
        Just end -> dateKey <= end
  in
    matchesType && matchesStatus && matchesCategory && matchesStart && matchesEnd

calendarItemContent :: CalendarItem -> CalendarItemContent
calendarItemContent (NewCalendarItem { content }) = content
calendarItemContent (ServerCalendarItem { content }) = content

normalizeHeader :: String -> String
normalizeHeader = StringCommon.trim >>> StringCommon.toLower

datePart :: String -> String
datePart raw =
  if String.length raw >= 10 then String.slice 0 10 raw else raw

exportItemsToCsv :: Array CalendarItem -> String
exportItemsToCsv items =
  let
    header = "type,titre,fenetre_debut,fenetre_fin,statut,categorie"
    rows = map exportCsvRow items
  in
    StringCommon.joinWith "\n" ([ header ] <> rows)

exportCsvRow :: CalendarItem -> String
exportCsvRow item =
  let
    content = calendarItemContent item
    category = fromMaybe "" content.category
  in
    StringCommon.joinWith "," $
      map csvEscape
        [ exportItemType content.itemType
        , content.title
        , content.windowStart
        , content.windowEnd
        , exportItemStatus content.status
        , category
        ]

csvEscape :: String -> String
csvEscape value =
  "\"" <> escapeQuotes value <> "\""

escapeQuotes :: String -> String
escapeQuotes raw =
  foldl
    (\acc ch -> if ch == '"' then acc <> "\"\"" else acc <> String.singleton ch)
    ""
    (String.toCharArray raw)

exportItemsToIcs :: Array CalendarItem -> String
exportItemsToIcs items =
  let
    header = [ "BEGIN:VCALENDAR", "VERSION:2.0", "PRODID:-//FAVS//EN" ]
    events = items >>= exportIcsEvent
  in
    StringCommon.joinWith "\n" (header <> events <> [ "END:VCALENDAR" ])

exportIcsEvent :: CalendarItem -> Array String
exportIcsEvent item =
  let
    content = calendarItemContent item
    start = toIcsDateTime content.windowStart
    end = toIcsDateTime content.windowEnd
    categoryLine =
      case content.category of
        Nothing -> []
        Just value -> [ "CATEGORIES:" <> value ]
  in
    [ "BEGIN:VEVENT"
    , "SUMMARY:" <> content.title
    , "DTSTART:" <> start
    , "DTEND:" <> end
    ]
      <> categoryLine
      <> [ "END:VEVENT" ]

toIcsDateTime :: String -> String
toIcsDateTime raw =
  if String.length raw >= 16 then
    String.slice 0 4 raw <>
    String.slice 5 7 raw <>
    String.slice 8 10 raw <>
    "T" <>
    String.slice 11 13 raw <>
    String.slice 14 16 raw
  else raw

exportItemType :: ItemType -> String
exportItemType Intention = "INTENTION"
exportItemType ScheduledBlock = "BLOC_PLANIFIE"

exportItemStatus :: ItemStatus -> String
exportItemStatus Todo = "TODO"
exportItemStatus EnCours = "EN_COURS"
exportItemStatus Fait = "FAIT"
exportItemStatus Annule = "ANNULE"
