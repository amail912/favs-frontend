module Calendar.ExImport.Import
  ( Mode(..)
  , Input(..)
  , Output(..)
  , CsvError
  , CsvResult
  , IcsError
  , IcsResult
  , component
  , parseCsv
  , parseIcs
  ) where

import Prelude hiding (div)

import Calendar.ExImport.Model (Item(..), ItemStatus(..), ItemType(..))
import Calendar.Recurrence (RecurrenceRule(..))
import Data.Array (any, elem, filter, findIndex, findMap, index, last, length, mapWithIndex, mapMaybe, null, uncons)
import Data.Bifunctor (lmap)
import Data.Date (Date)
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Int as Int
import Data.Lens (Lens', (.~), lens)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String.CodeUnits as String
import Data.String.Common as StringCommon
import Data.String.Pattern (Pattern(..))
import Effect.Aff (Aff)
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, get, mkComponent, mkEval, modify_, raise) as H
import Halogen.HTML (HTML, button, div, section, text, textarea)
import Halogen.HTML.Events (onClick, onValueChange)
import Halogen.HTML.Properties (placeholder, value)
import Helpers.DateTime as DateTime
import Ui.AgendaRender (renderPanelHeader)
import Ui.Utils (class_)

newtype Input = Input
  { mode :: Mode }

data Output = ApplyItems (Array Item)

data Mode
  = Csv
  | Ics

newtype State = State
  { mode :: Mode
  , input :: String
  , result :: Maybe Result
  }

_csvInput :: Lens' State String
_csvInput = lens
    (\(State state) -> state.input)
    (\(State state) input -> State (state { input = input }))

_result :: Lens' State (Maybe Result)
_result = lens
    (\(State state) -> state.result)
    (\(State state) result -> State (state { result = result }))

data Result
  = CsvResult CsvResult
  | IcsResult IcsResult

data Action
  = Receive Input
  | InputChanged String
  | Parse
  | Apply
  | Clear

component :: forall q. H.Component q Input Output Aff
component =
  H.mkComponent
    { initialState: \(Input input) -> State { mode: input.mode, input: "", result: Nothing }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, receive = Just <<< Receive }
    }

handleAction :: Action -> H.HalogenM State Action () Output Aff Unit
handleAction = case _ of
  Receive (Input input) -> H.modify_ (\_ -> State { mode: input.mode, input: "", result: Nothing })
  InputChanged raw -> H.modify_ (_csvInput .~ raw)
  Parse -> do
    st <- H.get
    let
      nextResult =
        case st of
          State { mode: Csv, input } -> CsvResult (parseCsv input)
          State { mode: Ics, input } -> IcsResult (parseIcs input)
    H.modify_ (_result .~ Just nextResult)
  Apply -> do
    st <- H.get
    case st of
      State { result: Just (CsvResult res) } -> applyItems res.items
      State { result: Just (IcsResult res) } -> applyItems res.items
      _ -> pure unit
  Clear -> H.modify_ ((_csvInput .~ "") <<< (_result .~ Nothing))
  where
  applyItems items =
    if null items then pure unit
    else do
      H.raise (ApplyItems items)
      H.modify_ ((_csvInput .~ "") <<< (_result .~ Nothing))

render :: forall m. State -> H.ComponentHTML Action () m
render (State state) =
  let
    config =
      case state.mode of
        Csv ->
          { title: "Import CSV"
          , subtitle: "Colonnes minimales: type, titre, fenetre_debut, fenetre_fin."
          , placeholder: "Collez votre CSV ici..."
          }
        Ics ->
          { title: "Import ICS"
          , subtitle: "Support basique: SUMMARY, DTSTART, DTEND."
          , placeholder: "Collez votre fichier ICS ici..."
          }
    resultContent =
      case state.result of
        Nothing -> text ""
        Just (CsvResult result) -> renderCsvResult result
        Just (IcsResult result) -> renderIcsResult result
  in
    section [ class_ "calendar-import" ]
      [ renderPanelHeader
          { baseClass: "calendar-import"
          , title: config.title
          , subtitle: config.subtitle
          }
          []
      , textarea
          [ class_ "form-control calendar-import-textarea"
          , placeholder config.placeholder
          , value state.input
          , onValueChange InputChanged
          ]
      , div [ class_ "calendar-import-actions" ]
          [ button [ class_ "btn btn-sm btn-outline-primary", onClick (const Parse) ] [ text "Analyser" ]
          , button [ class_ "btn btn-sm btn-outline-secondary", onClick (const Clear) ] [ text "Effacer" ]
          , button [ class_ "btn btn-sm btn-success", onClick (const Apply) ] [ text "Ajouter a la liste" ]
          ]
      , resultContent
      ]

renderCsvResult :: forall w action. CsvResult -> HTML w action
renderCsvResult result =
  let
    okCount = length result.items
    errorCount = length result.errors
  in
    div [ class_ "calendar-import-result" ]
      [ div [ class_ "calendar-import-summary" ]
          [ text $ "Valides: " <> show okCount <> " • Erreurs: " <> show errorCount ]
      , if null result.errors then text "" else renderCsvErrors result.errors
      ]

renderCsvErrors :: forall w action. Array CsvError -> HTML w action
renderCsvErrors errors =
  div [ class_ "calendar-import-errors" ]
    (map renderCsvError errors)

renderCsvError :: forall w action. CsvError -> HTML w action
renderCsvError err =
  div [ class_ "calendar-import-error" ]
    [ text $ "Ligne " <> show err.rowNumber <> ": " <> err.message ]

renderIcsResult :: forall w action. IcsResult -> HTML w action
renderIcsResult result =
  let
    okCount = length result.items
    errorCount = length result.errors
  in
    div [ class_ "calendar-import-result" ]
      [ div [ class_ "calendar-import-summary" ]
          [ text $ "Valides: " <> show okCount <> " • Erreurs: " <> show errorCount ]
      , if null result.errors then text "" else renderIcsErrors result.errors
      ]

renderIcsErrors :: forall w action. Array IcsError -> HTML w action
renderIcsErrors errors =
  div [ class_ "calendar-import-errors" ]
    (map renderIcsError errors)

renderIcsError :: forall w action. IcsError -> HTML w action
renderIcsError err =
  div [ class_ "calendar-import-error" ]
    [ text $ "Evenement " <> show err.eventIndex <> ": " <> err.message ]

type CsvError =
  { rowNumber :: Int
  , message :: String
  }

type CsvResult =
  { items :: Array Item
  , errors :: Array CsvError
  }

type IcsError =
  { eventIndex :: Int
  , message :: String
  }

type IcsResult =
  { items :: Array Item
  , errors :: Array IcsError
  }

parseCsv :: String -> CsvResult
parseCsv raw =
  let
    lines = map stripCR (StringCommon.split (Pattern "\n") raw)
    indexed = mapWithIndex (\idx -> { row: idx + 1, raw: _ }) lines
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
    sourceItemIdIdx = findFor [ "source_item_id" ]
    actualDurationIdx = findFor [ "actual_duration_minutes" ]
    recurrenceRuleTypeIdx = findFor [ "recurrence_rule_type" ]
    recurrenceRuleIntervalIdx = findFor [ "recurrence_rule_interval_days" ]
    recurrenceExceptionIdx = findFor [ "recurrence_exception_dates" ]
  in
    case
      { typeIdx
      , titleIdx
      , startIdx
      , endIdx
      , categoryIdx
      , statusIdx
      , sourceItemIdIdx
      , actualDurationIdx
      , recurrenceRuleTypeIdx
      , recurrenceRuleIntervalIdx
      , recurrenceExceptionIdx
      }
      of
      { typeIdx: Just t
      , titleIdx: Just ti
      , startIdx: Just s
      , endIdx: Just e
      , categoryIdx: Just c
      , statusIdx: Just st
      , sourceItemIdIdx: Just source
      , actualDurationIdx: Just duration
      , recurrenceRuleTypeIdx: Just ruleType
      , recurrenceRuleIntervalIdx: Just ruleInterval
      , recurrenceExceptionIdx: Just exceptions
      } ->
        Right
          { typeIdx: t
          , titleIdx: ti
          , startIdx: s
          , endIdx: e
          , categoryIdx: c
          , statusIdx: st
          , sourceItemIdIdx: source
          , actualDurationIdx: duration
          , recurrenceRuleTypeIdx: ruleType
          , recurrenceRuleIntervalIdx: ruleInterval
          , recurrenceExceptionIdx: exceptions
          }
      _ ->
        Left
          "Colonnes manquantes: type, titre, fenetre_debut, fenetre_fin, categorie, statut, source_item_id, actual_duration_minutes, recurrence_rule_type, recurrence_rule_interval_days, recurrence_exception_dates."

type CsvHeader =
  { typeIdx :: Int
  , titleIdx :: Int
  , startIdx :: Int
  , endIdx :: Int
  , categoryIdx :: Int
  , statusIdx :: Int
  , sourceItemIdIdx :: Int
  , actualDurationIdx :: Int
  , recurrenceRuleTypeIdx :: Int
  , recurrenceRuleIntervalIdx :: Int
  , recurrenceExceptionIdx :: Int
  }

parseCsvRows :: CsvHeader -> Array { row :: Int, raw :: String } -> CsvResult
parseCsvRows header rows =
  foldl parseRow { items: [], errors: [] } rows
  where
  parseRow acc row =
    let
      fields = splitCsvLine row.raw
      fieldAt idx = index fields idx
    in
      case
        { typeVal: fieldAt header.typeIdx
        , titleVal: fieldAt header.titleIdx
        , startVal: fieldAt header.startIdx
        , endVal: fieldAt header.endIdx
        }
        of
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

parseCsvItem :: CsvHeader -> Array String -> String -> String -> String -> String -> Either String Item
parseCsvItem header fields typeVal titleVal startVal endVal = do
  itemType <- parseCsvItemType typeVal
  status <- parseCsvStatus (lookupField header.statusIdx fields)
  let
    title = StringCommon.trim titleVal
    windowStartRaw = StringCommon.trim startVal
    windowEndRaw = StringCommon.trim endVal
    category = lookupField header.categoryIdx fields >>= toOptionalString
    sourceItemId = lookupField header.sourceItemIdIdx fields >>= toOptionalString
    actualDurationRaw = lookupField header.actualDurationIdx fields >>= toOptionalString
    ruleTypeRaw = lookupField header.recurrenceRuleTypeIdx fields >>= toOptionalString
    ruleIntervalRaw = lookupField header.recurrenceRuleIntervalIdx fields >>= toOptionalString
    exceptionRaw = lookupField header.recurrenceExceptionIdx fields >>= toOptionalString
    exceptions = splitExceptions exceptionRaw
  if title == "" then Left "Le titre est vide."
  else
    case parseDateTimeLocal windowStartRaw of
      Nothing -> Left "Début invalide (format YYYY-MM-DDTHH:MM)."
      Just windowStart ->
        case parseDateTimeLocal windowEndRaw of
          Nothing -> Left "Fin invalide (format YYYY-MM-DDTHH:MM)."
          Just windowEnd ->
            if windowEnd <= windowStart then Left "La fin doit être après le début."
            else
              case parseActualDuration actualDurationRaw of
                Left err -> Left err
                Right actualDurationMinutes ->
                  case parseRecurrenceRule ruleTypeRaw ruleIntervalRaw of
                    Left err -> Left err
                    Right recurrenceRule ->
                      case validateExceptions exceptions of
                        Left err -> Left err
                        Right validExceptions ->
                          Right $ Item
                            { itemType
                            , title
                            , windowStart
                            , windowEnd
                            , status
                            , sourceItemId
                            , actualDurationMinutes
                            , category
                            , recurrenceRule
                            , recurrenceExceptionDates: validExceptions
                            }

lookupField :: Int -> Array String -> Maybe String
lookupField idx fields = index fields idx

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
        "in_progress" -> Right InProgress
        "done" -> Right Done
        "canceled" -> Right Canceled
        _ -> Left "Statut invalide (TODO, IN_PROGRESS, DONE, CANCELED)."

parseActualDuration :: Maybe String -> Either String (Maybe Int)
parseActualDuration raw =
  case raw of
    Nothing -> Right Nothing
    Just value ->
      case Int.fromString value of
        Nothing -> Left "Durée réelle invalide."
        Just minutes | minutes <= 0 -> Left "Durée réelle invalide."
        Just minutes -> Right (Just minutes)

parseRecurrenceRule :: Maybe String -> Maybe String -> Either String (Maybe RecurrenceRule)
parseRecurrenceRule rawType rawInterval =
  case rawType of
    Nothing ->
      case rawInterval of
        Nothing -> Right Nothing
        Just _ -> Left "Intervalle de récurrence sans type."
    Just ruleType ->
      case normalizeHeader ruleType of
        "daily" ->
          if rawInterval == Nothing then Right (Just Daily)
          else Left "Intervalle invalide pour daily."
        "weekly" ->
          if rawInterval == Nothing then Right (Just Weekly)
          else Left "Intervalle invalide pour weekly."
        "monthly" ->
          if rawInterval == Nothing then Right (Just Monthly)
          else Left "Intervalle invalide pour monthly."
        "yearly" ->
          if rawInterval == Nothing then Right (Just Yearly)
          else Left "Intervalle invalide pour yearly."
        "every" ->
          case rawInterval >>= Int.fromString of
            Nothing -> Left "Intervalle requis pour every."
            Just days | days <= 0 -> Left "Intervalle requis pour every."
            Just days -> Right (Just (EveryXDays days))
        _ -> Left "Type de récurrence invalide (daily, weekly, monthly, yearly, every)."

splitExceptions :: Maybe String -> Array String
splitExceptions raw =
  case raw of
    Nothing -> []
    Just value ->
      StringCommon.split (Pattern ";") value
        # map StringCommon.trim
        # filter (\entry -> entry /= "")

validateExceptions :: Array String -> Either String (Array Date)
validateExceptions exceptions = parseExceptionDates exceptions

parseExceptionDates :: Array String -> Either String (Array Date)
parseExceptionDates exceptions =
  let
    parsed = map DateTime.parseLocalDate exceptions
  in
    if any (_ == Nothing) parsed then Left "Exception invalide (format YYYY-MM-DD)."
    else Right (mapMaybe identity parsed)

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
  let
    len = String.length line
  in
    if len > 0 && String.charAt (len - 1) line == Just '\r' then String.slice 0 (len - 1) line
    else line

type IcsEventDraft =
  { summary :: Maybe String
  , dtStart :: Maybe String
  , dtEnd :: Maybe String
  , itemType :: Maybe String
  , status :: Maybe String
  , sourceItemId :: Maybe String
  , actualDurationMinutes :: Maybe String
  , category :: Maybe String
  , rrule :: Maybe String
  , exdates :: Array String
  }

parseIcs :: String -> IcsResult
parseIcs raw =
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
        state
          { current =
              Just
                { summary: Nothing
                , dtStart: Nothing
                , dtEnd: Nothing
                , itemType: Nothing
                , status: Nothing
                , sourceItemId: Nothing
                , actualDurationMinutes: Nothing
                , category: Nothing
                , rrule: Nothing
                , exdates: []
                }
          }
      "END:VEVENT" ->
        case state.current of
          Nothing -> state
          Just event ->
            let
              nextIndex = state.index + 1
            in
              case buildIcsItem nextIndex event of
                Left err -> state { errors = state.errors <> [ err ], current = Nothing, index = nextIndex }
                Right item -> state { items = state.items <> [ item ], current = Nothing, index = nextIndex }
      _ ->
        case state.current of
          Nothing -> state
          Just event ->
            let
              key = extractIcsKey line
              value = extractIcsValue line
              trimmed = StringCommon.trim value
              updated =
                case key of
                  "SUMMARY" -> event { summary = toOptionalString value }
                  "DTSTART" -> event { dtStart = Just value }
                  "DTEND" -> event { dtEnd = Just value }
                  "X-FAVS-TYPE" -> event { itemType = toOptionalString value }
                  "X-FAVS-STATUS" -> event { status = toOptionalString value }
                  "X-FAVS-SOURCE-ITEM-ID" -> event { sourceItemId = Just trimmed }
                  "X-FAVS-ACTUAL-DURATION-MINUTES" -> event { actualDurationMinutes = Just trimmed }
                  "CATEGORIES" -> event { category = toOptionalString value }
                  "RRULE" -> event { rrule = toOptionalString value }
                  "EXDATE" -> event { exdates = event.exdates <> splitIcsExdates trimmed }
                  _ -> event
            in
              state { current = Just updated }

buildIcsItem :: Int -> IcsEventDraft -> Either IcsError Item
buildIcsItem index event = do
  title <- maybe (Left { eventIndex: index, message: "SUMMARY manquant." }) Right event.summary
  startRaw <- maybe (Left { eventIndex: index, message: "DTSTART manquant." }) Right event.dtStart
  endRaw <- maybe (Left { eventIndex: index, message: "DTEND manquant." }) Right event.dtEnd
  typeRaw <- maybe (Left { eventIndex: index, message: "X-FAVS-TYPE manquant." }) Right event.itemType
  statusRaw <- maybe (Left { eventIndex: index, message: "X-FAVS-STATUS manquant." }) Right event.status
  sourceRaw <- maybe (Left { eventIndex: index, message: "X-FAVS-SOURCE-ITEM-ID manquant." }) Right event.sourceItemId
  durationRaw <- maybe (Left { eventIndex: index, message: "X-FAVS-ACTUAL-DURATION-MINUTES manquant." }) Right event.actualDurationMinutes
  windowStart <- maybe (Left { eventIndex: index, message: "DTSTART invalide." }) Right (parseIcsDateTime startRaw)
  windowEnd <- maybe (Left { eventIndex: index, message: "DTEND invalide." }) Right (parseIcsDateTime endRaw)
  itemType <- lmap ({ eventIndex: index, message: _ }) (parseIcsItemType typeRaw)
  status <- lmap ({ eventIndex: index, message: _ }) (parseIcsStatus statusRaw)
  sourceItemId <- lmap ({ eventIndex: index, message: _ }) (parseIcsSourceItemId sourceRaw)
  actualDurationMinutes <- lmap ({ eventIndex: index, message: _ }) (parseIcsActualDuration durationRaw)
  recurrenceRule <- lmap ({ eventIndex: index, message: _ }) (parseIcsRrule event.rrule)
  recurrenceExceptionDates <- lmap ({ eventIndex: index, message: _ }) (parseIcsExceptions event.exdates)
  if windowEnd <= windowStart then Left { eventIndex: index, message: "La fin doit être après le début." }
  else
    Right $ Item
      { itemType
      , title
      , windowStart
      , windowEnd
      , status
      , sourceItemId
      , actualDurationMinutes
      , category: event.category
      , recurrenceRule
      , recurrenceExceptionDates
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

parseIcsDateTime :: String -> Maybe DateTime
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
        formatted = StringCommon.joinWith "" [ y, "-", m, "-", d, "T", hh, ":", mm ]
      in
        DateTime.parseLocalDateTime formatted

endsWithChar :: Char -> String -> Boolean
endsWithChar ch str =
  let
    len = String.length str
  in
    len > 0 && String.charAt (len - 1) str == Just ch

splitIcsExdates :: String -> Array String
splitIcsExdates raw =
  if raw == "" then []
  else StringCommon.split (Pattern ",") raw

parseIcsItemType :: String -> Either String ItemType
parseIcsItemType raw =
  case normalizeHeader raw of
    "intention" -> Right Intention
    "bloc_planifie" -> Right ScheduledBlock
    _ -> Left "X-FAVS-TYPE invalide (INTENTION ou BLOC_PLANIFIE)."

parseIcsStatus :: String -> Either String ItemStatus
parseIcsStatus raw =
  case normalizeHeader raw of
    "todo" -> Right Todo
    "in_progress" -> Right InProgress
    "done" -> Right Done
    "canceled" -> Right Canceled
    _ -> Left "X-FAVS-STATUS invalide (TODO, IN_PROGRESS, DONE, CANCELED)."

parseIcsSourceItemId :: String -> Either String (Maybe String)
parseIcsSourceItemId raw =
  Right (toOptionalString raw)

parseIcsActualDuration :: String -> Either String (Maybe Int)
parseIcsActualDuration raw =
  case toOptionalString raw of
    Nothing -> Right Nothing
    Just trimmed ->
      case Int.fromString trimmed of
        Nothing -> Left "X-FAVS-ACTUAL-DURATION-MINUTES invalide."
        Just minutes | minutes <= 0 -> Left "X-FAVS-ACTUAL-DURATION-MINUTES invalide."
        Just minutes -> Right (Just minutes)

parseIcsRrule :: Maybe String -> Either String (Maybe RecurrenceRule)
parseIcsRrule Nothing = Right Nothing
parseIcsRrule (Just raw) =
  let
    parts = StringCommon.split (Pattern ";") raw
    freq = findMap (stripPrefix "FREQ=") parts
    interval = findMap (stripPrefix "INTERVAL=") parts
  in
    case map StringCommon.toUpper freq of
      Nothing -> Left "RRULE invalide."
      Just "DAILY" ->
        case interval >>= Int.fromString of
          Nothing -> Right (Just Daily)
          Just value | value <= 0 -> Left "RRULE invalide."
          Just value | value == 1 -> Right (Just Daily)
          Just value -> Right (Just (EveryXDays value))
      Just "WEEKLY" -> if interval == Nothing then Right (Just Weekly) else Left "RRULE invalide."
      Just "MONTHLY" -> if interval == Nothing then Right (Just Monthly) else Left "RRULE invalide."
      Just "YEARLY" -> if interval == Nothing then Right (Just Yearly) else Left "RRULE invalide."
      _ -> Left "RRULE invalide."
  where
  stripPrefix prefix value =
    if String.slice 0 (String.length prefix) value == prefix then
      Just (String.slice (String.length prefix) (String.length value) value)
    else
      Nothing

parseIcsExceptions :: Array String -> Either String (Array Date)
parseIcsExceptions raw =
  let
    parsed = map parseIcsDate raw
  in
    if any (_ == Nothing) parsed then Left "EXDATE invalide."
    else Right (mapMaybe identity parsed)

parseIcsDate :: String -> Maybe Date
parseIcsDate raw =
  let
    trimmed = StringCommon.trim raw
    cleaned = if endsWithChar 'Z' trimmed then String.slice 0 (String.length trimmed - 1) trimmed else trimmed
    base =
      if String.length cleaned >= 8 then
        if String.charAt 8 cleaned == Just 'T' then String.slice 0 8 cleaned else String.slice 0 8 cleaned
      else
        ""
    formatted =
      if String.length base == 8 then
        String.slice 0 4 base <> "-" <> String.slice 4 6 base <> "-" <> String.slice 6 8 base
      else ""
  in
    if formatted == "" then Nothing
    else DateTime.parseLocalDate formatted

normalizeHeader :: String -> String
normalizeHeader = StringCommon.trim >>> StringCommon.toLower

toOptionalString :: String -> Maybe String
toOptionalString raw =
  let
    trimmed = StringCommon.trim raw
  in
    if trimmed == "" then Nothing else Just trimmed

parseDateTimeLocal :: String -> Maybe DateTime
parseDateTimeLocal = DateTime.parseLocalDateTime
