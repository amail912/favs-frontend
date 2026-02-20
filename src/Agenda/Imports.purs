module Agenda.Imports
  ( parseCsvImport
  , parseIcsImport
  ) where

import Prelude

import Agenda.Model (CalendarItem(..), CsvImportResult, IcsImportError, IcsImportResult, ItemStatus(..), ItemType(..))
import Data.Array (elem, filter, findIndex, foldl, index, last, mapWithIndex, uncons)
import Data.Either (Either(..))
import Data.Foldable (all)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String.CodeUnits as String
import Data.String.Common as StringCommon
import Data.String (toLower)
import Data.String.Pattern (Pattern(..))

parseCsvImport :: String -> CsvImportResult
parseCsvImport raw =
  let
    lines = map stripCR (StringCommon.split (Pattern "\n") raw)
    indexed = mapWithIndex (\idx line -> { row: idx + 1, raw: line }) lines
    nonEmpty = filter (\line -> StringCommon.trim line.raw /= "") indexed
  in
    case uncons nonEmpty of
      Nothing -> { items: [], errors: [ { rowNumber: 1, message: "CSV vide." } ] }
      Just { head: headerLine, tail: dataLines } ->
        case resolveCsvHeader headerLine.raw of
          Left err -> { items: [], errors: [ { rowNumber: headerLine.row, message: err } ] }
          Right header -> parseCsvRows header dataLines

resolveCsvHeader :: String -> Either String CsvHeader
resolveCsvHeader rawHeader =
  let
    headers = map normalizeHeader (splitCsvLine rawHeader)
    findFor names = findIndex (\name -> elem name names) headers
    typeIdx = findFor [ "type" ]
    titleIdx = findFor [ "titre", "title" ]
    startIdx = findFor [ "fenetre_debut", "window_start", "start" ]
    endIdx = findFor [ "fenetre_fin", "window_end", "end" ]
    categoryIdx = findFor [ "categorie", "category" ]
    statusIdx = findFor [ "statut", "status" ]
  in
    case { typeIdx, titleIdx, startIdx, endIdx } of
      { typeIdx: Just t, titleIdx: Just ti, startIdx: Just s, endIdx: Just e } ->
        Right { typeIdx: t, titleIdx: ti, startIdx: s, endIdx: e, categoryIdx, statusIdx }
      _ ->
        Left "Colonnes minimales manquantes: type, titre, fenetre_debut, fenetre_fin."

type CsvHeader =
  { typeIdx :: Int
  , titleIdx :: Int
  , startIdx :: Int
  , endIdx :: Int
  , categoryIdx :: Maybe Int
  , statusIdx :: Maybe Int
  }

parseCsvRows :: CsvHeader -> Array { row :: Int, raw :: String } -> CsvImportResult
parseCsvRows header rows =
  foldl parseRow { items: [], errors: [] } rows
  where
  parseRow acc row =
    let
      fields = splitCsvLine row.raw
      fieldAt idx = index fields idx
    in
      case { typeVal: fieldAt header.typeIdx
           , titleVal: fieldAt header.titleIdx
           , startVal: fieldAt header.startIdx
           , endVal: fieldAt header.endIdx
           } of
        { typeVal: Just typeVal
        , titleVal: Just titleVal
        , startVal: Just startVal
        , endVal: Just endVal
        } ->
          case parseCsvItem header fields typeVal titleVal startVal endVal of
            Left err -> acc { errors = acc.errors <> [ { rowNumber: row.row, message: err } ] }
            Right item -> acc { items = acc.items <> [ item ] }
        _ ->
          acc { errors = acc.errors <> [ { rowNumber: row.row, message: "Colonnes manquantes pour cette ligne." } ] }

parseCsvItem :: CsvHeader -> Array String -> String -> String -> String -> String -> Either String CalendarItem
parseCsvItem header fields typeVal titleVal startVal endVal = do
  itemType <- parseCsvItemType typeVal
  status <- parseCsvStatus (lookupField header.statusIdx fields)
  let
    title = StringCommon.trim titleVal
    windowStart = StringCommon.trim startVal
    windowEnd = StringCommon.trim endVal
    category = lookupField header.categoryIdx fields >>= toOptionalString
  if title == "" then Left "Le titre est vide."
  else if not (isDateTimeLocal windowStart) then Left "Debut invalide (format YYYY-MM-DDTHH:MM)."
  else if not (isDateTimeLocal windowEnd) then Left "Fin invalide (format YYYY-MM-DDTHH:MM)."
  else if windowEnd <= windowStart then Left "La fin doit etre apres le debut."
  else
    Right $ NewCalendarItem
      { content:
          { itemType
          , title
          , windowStart
          , windowEnd
          , status
          , sourceItemId: Nothing
          , actualDurationMinutes: Nothing
          , category
          , recurrenceRule: Nothing
          , recurrenceExceptionDates: []
          }
      }

lookupField :: Maybe Int -> Array String -> Maybe String
lookupField idx fields = idx >>= \i -> index fields i

parseCsvItemType :: String -> Either String ItemType
parseCsvItemType raw =
  case normalizeHeader raw of
    "intention" -> Right Intention
    "bloc_planifie" -> Right ScheduledBlock
    "scheduled_block" -> Right ScheduledBlock
    _ -> Left "Type invalide (INTENTION ou BLOC_PLANIFIE)."

parseCsvStatus :: Maybe String -> Either String ItemStatus
parseCsvStatus raw =
  case raw of
    Nothing -> Right Todo
    Just value | StringCommon.trim value == "" -> Right Todo
    Just value ->
      case normalizeHeader value of
        "todo" -> Right Todo
        "en_cours" -> Right EnCours
        "fait" -> Right Fait
        "annule" -> Right Annule
        _ -> Left "Statut invalide (TODO, EN_COURS, FAIT, ANNULE)."

normalizeHeader :: String -> String
normalizeHeader = toLower <<< StringCommon.trim

toOptionalString :: String -> Maybe String
toOptionalString raw =
  let trimmed = StringCommon.trim raw
  in if trimmed == "" then Nothing else Just trimmed

splitCsvLine :: String -> Array String
splitCsvLine raw =
  parseChars (String.toCharArray raw) { field: "", fields: [], inQuote: false }
  where
  parseChars chars state =
    case uncons chars of
      Nothing -> state.fields <> [ state.field ]
      Just { head, tail } ->
        if head == '"' then
          case uncons tail of
            Just { head: next, tail: rest } | state.inQuote && next == '"' ->
              parseChars rest state { field = state.field <> String.singleton '"' }
            _ ->
              parseChars tail state { inQuote = not state.inQuote }
        else if head == ',' && not state.inQuote then
          parseChars tail state { fields = state.fields <> [ state.field ], field = "" }
        else
          parseChars tail state { field = state.field <> String.singleton head }

stripCR :: String -> String
stripCR line =
  let len = String.length line
  in if len > 0 && String.charAt (len - 1) line == Just '\r'
     then String.slice 0 (len - 1) line
     else line

type IcsEventDraft =
  { summary :: Maybe String
  , dtStart :: Maybe String
  , dtEnd :: Maybe String
  }

parseIcsImport :: String -> IcsImportResult
parseIcsImport raw =
  let
    lines = map stripCR (StringCommon.split (Pattern "\n") raw)
    initial =
      { current: Nothing
      , items: []
      , errors: []
      , index: 0
      }
    final = foldl parseIcsLine initial lines
  in
    { items: final.items, errors: final.errors }
  where
  parseIcsLine state line =
    case StringCommon.trim line of
      "BEGIN:VEVENT" ->
        state { current = Just { summary: Nothing, dtStart: Nothing, dtEnd: Nothing } }
      "END:VEVENT" ->
        case state.current of
          Nothing -> state
          Just event ->
            let nextIndex = state.index + 1
            in case buildIcsItem nextIndex event of
                Left err -> state { errors = state.errors <> [ err ], current = Nothing, index = nextIndex }
                Right item -> state { items = state.items <> [ item ], current = Nothing, index = nextIndex }
      _ ->
        case state.current of
          Nothing -> state
          Just event ->
            let
              key = extractIcsKey line
              value = extractIcsValue line
              updated =
                case key of
                  "SUMMARY" -> event { summary = toOptionalString value }
                  "DTSTART" -> event { dtStart = Just value }
                  "DTEND" -> event { dtEnd = Just value }
                  _ -> event
            in
              state { current = Just updated }

buildIcsItem :: Int -> IcsEventDraft -> Either IcsImportError CalendarItem
buildIcsItem index event = do
  title <- maybe (Left { eventIndex: index, message: "SUMMARY manquant." }) Right event.summary
  startRaw <- maybe (Left { eventIndex: index, message: "DTSTART manquant." }) Right event.dtStart
  endRaw <- maybe (Left { eventIndex: index, message: "DTEND manquant." }) Right event.dtEnd
  windowStart <- maybe (Left { eventIndex: index, message: "DTSTART invalide." }) Right (parseIcsDateTime startRaw)
  windowEnd <- maybe (Left { eventIndex: index, message: "DTEND invalide." }) Right (parseIcsDateTime endRaw)
  if windowEnd <= windowStart then Left { eventIndex: index, message: "La fin doit etre apres le debut." }
  else
    Right $ NewCalendarItem
      { content:
          { itemType: ScheduledBlock
          , title
          , windowStart
          , windowEnd
          , status: Todo
          , sourceItemId: Nothing
          , actualDurationMinutes: Nothing
          , category: Nothing
          , recurrenceRule: Nothing
          , recurrenceExceptionDates: []
          }
      }

extractIcsKey :: String -> String
extractIcsKey line =
  case uncons (StringCommon.split (Pattern ":") line) of
    Nothing -> ""
    Just { head: first } ->
      case uncons (StringCommon.split (Pattern ";") first) of
        Nothing -> ""
        Just { head: key } -> StringCommon.trim key

extractIcsValue :: String -> String
extractIcsValue line =
  fromMaybe "" (last (StringCommon.split (Pattern ":") line))

parseIcsDateTime :: String -> Maybe String
parseIcsDateTime raw =
  let
    trimmed = StringCommon.trim raw
    cleaned = if endsWithChar 'Z' trimmed then String.slice 0 (String.length trimmed - 1) trimmed else trimmed
  in
    if String.length cleaned < 13 then Nothing
    else if String.charAt 8 cleaned /= Just 'T' then Nothing
    else
      let
        y = String.slice 0 4 cleaned
        m = String.slice 4 6 cleaned
        d = String.slice 6 8 cleaned
        hh = String.slice 9 11 cleaned
        mm = String.slice 11 13 cleaned
      in
        Just $ y <> "-" <> m <> "-" <> d <> "T" <> hh <> ":" <> mm
  where
  endsWithChar ch str =
    let len = String.length str
    in len > 0 && String.charAt (len - 1) str == Just ch

isDateTimeLocal :: String -> Boolean
isDateTimeLocal raw =
  String.length raw == 16
    && matchesAt 4 '-'
    && matchesAt 7 '-'
    && matchesAt 10 'T'
    && matchesAt 13 ':'
    && allDigitsAt [ 0, 1, 2, 3, 5, 6, 8, 9, 11, 12, 14, 15 ]
  where
  matchesAt :: Int -> Char -> Boolean
  matchesAt idx expected =
    case String.charAt idx raw of
      Just ch -> ch == expected
      Nothing -> false

  allDigitsAt :: Array Int -> Boolean
  allDigitsAt = all (\idx -> maybe false isDigitChar (String.charAt idx raw))

  isDigitChar :: Char -> Boolean
  isDigitChar ch = ch >= '0' && ch <= '9'
