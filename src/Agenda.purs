module Agenda
  ( component
  , CalendarItem(..)
  , CalendarItemContent
  , IntentionDraft
  , ItemStatus(..)
  , ItemType(..)
  , SortMode(..)
  , RecurrenceRule(..)
  , StepDependency(..)
  , RoutineTemplate
  , RoutineTemplateStep
  , RoutineInstance
  , RoutineInstanceStep
  , ValidationError(..)
  , applyOfflineMutation
  , durationMinutesBetween
  , detectConflictGroups
  , detectConflictIds
  , generateOccurrencesForMonth
  , instantiateRoutine
  , sortItems
  , toNewIntention
  , toScheduledBlock
  , validateIntention
  ) where

import Prelude hiding (div)

import Affjax (Error, printError)
import Affjax.RequestBody (RequestBody(..))
import Affjax.ResponseFormat (json)
import Affjax.Web (Response, post)
import Affjax.Web (get) as Affjax
import Control.Alt ((<|>))
import Control.Monad.Except (ExceptT(..), runExceptT, withExceptT)
import Control.Monad.RWS (get, modify_)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut.Core (Json, jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:), (.:?))
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Array (elem, filter, find, foldM, index, length, mapMaybe, mapWithIndex, nub, null, sortBy, uncons)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Foldable (all, foldl)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits as String
import Data.String.Common as StringCommon
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Date (canonicalDate, day, exactDate, month, year)
import Data.DateTime (DateTime(..), adjust, date, diff, time)
import Data.Enum (fromEnum, toEnum)
import Data.Time (Time(..), hour, minute)
import Data.Time.Duration (Days(..), Minutes(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Effect.Now (nowDateTime)
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, mkComponent, mkEval) as H
import Halogen.HTML (HTML, button, div, h2, input, li, option, section, select, text, ul)
import Halogen.HTML.Events (onClick, onDragEnd, onDragOver, onDragStart, onDrop, onValueChange)
import Halogen.HTML.Properties (IProp, draggable, placeholder, type_, value)
import Utils (class_)
import Web.Event.Event (preventDefault)
import Web.HTML.Event.DragEvent (DragEvent, toEvent)

type NoOutput = Void
type AgendaAppM = H.HalogenM State Action () NoOutput Aff
type ErrorAgendaAppM = ExceptT FatalError AgendaAppM

data ItemType = Intention | ScheduledBlock
derive instance itemTypeGeneric :: Generic ItemType _
derive instance itemTypeEq :: Eq ItemType
instance itemTypeShow :: Show ItemType where
  show = genericShow

data ItemStatus = Todo | EnCours | Fait | Annule
derive instance itemStatusGeneric :: Generic ItemStatus _
derive instance itemStatusEq :: Eq ItemStatus
instance itemStatusShow :: Show ItemStatus where
  show = genericShow

data RecurrenceRule
  = RecurrenceDaily
  | RecurrenceWeekly
  | RecurrenceMonthly
  | RecurrenceYearly
  | RecurrenceEveryXDays Int

derive instance recurrenceRuleGeneric :: Generic RecurrenceRule _
derive instance recurrenceRuleEq :: Eq RecurrenceRule
instance recurrenceRuleShow :: Show RecurrenceRule where
  show = genericShow

data StepDependency
  = StartAfterEnd { stepId :: String, offsetMinutes :: Int }
  | StartBeforeStart { stepId :: String, offsetMinutes :: Int }

derive instance stepDependencyGeneric :: Generic StepDependency _
derive instance stepDependencyEq :: Eq StepDependency
instance stepDependencyShow :: Show StepDependency where
  show = genericShow

type RoutineTemplate =
  { id :: String
  , name :: String
  , steps :: Array RoutineTemplateStep
  }

type RoutineTemplateStep =
  { id :: String
  , title :: String
  , windowStart :: String
  , windowEnd :: String
  , dependsOn :: Maybe StepDependency
  }

type RoutineInstance =
  { templateId :: String
  , steps :: Array RoutineInstanceStep
  }

type RoutineInstanceStep =
  { id :: String
  , title :: String
  , windowStart :: String
  , windowEnd :: String
  , sourceStepId :: String
  }

type CalendarItemContent =
  { itemType :: ItemType
  , title :: String
  , windowStart :: String
  , windowEnd :: String
  , status :: ItemStatus
  , sourceItemId :: Maybe String
  , actualDurationMinutes :: Maybe Int
  , category :: Maybe String
  , recurrenceRule :: Maybe RecurrenceRule
  , recurrenceExceptionDates :: Array String
  }

data CalendarItem
  = NewCalendarItem { content :: CalendarItemContent }
  | ServerCalendarItem { content :: CalendarItemContent, id :: String }

derive instance calendarItemGeneric :: Generic CalendarItem _
derive instance calendarItemEq :: Eq CalendarItem
instance calendarItemShow :: Show CalendarItem where
  show = genericShow

type IntentionDraft =
  { title :: String
  , windowStart :: String
  , windowEnd :: String
  , category :: String
  }

data ValidationError
  = TitleEmpty
  | WindowStartInvalid
  | WindowEndInvalid
  | WindowOrderInvalid

derive instance validationErrorGeneric :: Generic ValidationError _
derive instance validationErrorEq :: Eq ValidationError
instance validationErrorShow :: Show ValidationError where
  show = genericShow

emptyDraft :: IntentionDraft
emptyDraft =
  { title: ""
  , windowStart: ""
  , windowEnd: ""
  , category: ""
  }

toNewIntention :: IntentionDraft -> CalendarItem
toNewIntention { title, windowStart, windowEnd, category } =
  NewCalendarItem
    { content:
        { itemType: Intention
        , title
        , windowStart
        , windowEnd
        , status: Todo
        , sourceItemId: Nothing
        , actualDurationMinutes: Nothing
        , category: toOptional category
        , recurrenceRule: Nothing
        , recurrenceExceptionDates: []
        }
    }
  where
  toOptional raw =
    let trimmed = StringCommon.trim raw
    in if trimmed == "" then Nothing else Just trimmed

toScheduledBlock :: String -> CalendarItemContent -> CalendarItem
toScheduledBlock sourceId content =
  NewCalendarItem
    { content:
        content
          { itemType = ScheduledBlock
          , status = Todo
          , sourceItemId = Just sourceId
          }
    }

validateIntention :: IntentionDraft -> Either ValidationError IntentionDraft
validateIntention draft =
  case unit of
    _ | StringCommon.trim draft.title == "" -> Left TitleEmpty
    _ | not (isDateTimeLocal draft.windowStart) -> Left WindowStartInvalid
    _ | not (isDateTimeLocal draft.windowEnd) -> Left WindowEndInvalid
    _ | draft.windowEnd <= draft.windowStart -> Left WindowOrderInvalid
    _ -> Right draft

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

instance itemTypeEncodeJson :: EncodeJson ItemType where
  encodeJson Intention = encodeJson "INTENTION"
  encodeJson ScheduledBlock = encodeJson "BLOC_PLANIFIE"

instance itemTypeDecodeJson :: DecodeJson ItemType where
  decodeJson json = do
    str <- decodeJson json
    case str of
      "INTENTION" -> pure Intention
      "BLOC_PLANIFIE" -> pure ScheduledBlock
      _ -> Left $ UnexpectedValue json

instance itemStatusEncodeJson :: EncodeJson ItemStatus where
  encodeJson Todo = encodeJson "TODO"
  encodeJson EnCours = encodeJson "EN_COURS"
  encodeJson Fait = encodeJson "FAIT"
  encodeJson Annule = encodeJson "ANNULE"

instance itemStatusDecodeJson :: DecodeJson ItemStatus where
  decodeJson json = do
    str <- decodeJson json
    case str of
      "TODO" -> pure Todo
      "EN_COURS" -> pure EnCours
      "FAIT" -> pure Fait
      "ANNULE" -> pure Annule
      _ -> Left $ UnexpectedValue json

instance recurrenceRuleEncodeJson :: EncodeJson RecurrenceRule where
  encodeJson rule =
    case rule of
      RecurrenceDaily ->
        "type" := "DAILY" ~> jsonEmptyObject
      RecurrenceWeekly ->
        "type" := "WEEKLY" ~> jsonEmptyObject
      RecurrenceMonthly ->
        "type" := "MONTHLY" ~> jsonEmptyObject
      RecurrenceYearly ->
        "type" := "YEARLY" ~> jsonEmptyObject
      RecurrenceEveryXDays interval ->
        "type" := "EVERY_X_DAYS"
          ~> "interval_days" := interval
          ~> jsonEmptyObject

instance recurrenceRuleDecodeJson :: DecodeJson RecurrenceRule where
  decodeJson json = do
    obj <- decodeJson json
    kind <- obj .: "type"
    case kind of
      "DAILY" -> pure RecurrenceDaily
      "WEEKLY" -> pure RecurrenceWeekly
      "MONTHLY" -> pure RecurrenceMonthly
      "YEARLY" -> pure RecurrenceYearly
      "EVERY_X_DAYS" -> RecurrenceEveryXDays <$> obj .: "interval_days"
      _ -> Left $ UnexpectedValue json

instance calendarItemDecodeJson :: DecodeJson CalendarItem where
  decodeJson json = do
    obj <- decodeJson json
    itemType <- obj .: "type"
    title <- obj .: "titre"
    windowStart <- obj .: "fenetre_debut"
    windowEnd <- obj .: "fenetre_fin"
    status <- obj .: "statut"
    sourceItemId <- obj .:? "source_item_id"
    actualDurationMinutes <- obj .:? "duree_reelle_minutes"
    category <- obj .:? "categorie"
    recurrenceRule <- obj .:? "recurrence_rule"
    recurrenceExceptionDates <- obj .:? "recurrence_exception_dates"
    let
      content =
        { itemType
        , title
        , windowStart
        , windowEnd
        , status
        , sourceItemId
        , actualDurationMinutes
        , category
        , recurrenceRule
        , recurrenceExceptionDates: fromMaybe [] recurrenceExceptionDates
        }
    either (const $ pure $ NewCalendarItem { content })
           (\id -> pure $ ServerCalendarItem { content, id })
           (obj .: "id")

instance calendarItemEncodeJson :: EncodeJson CalendarItem where
  encodeJson (NewCalendarItem { content }) =
    encodeCalendarContent content
  encodeJson (ServerCalendarItem { content, id }) =
    "id" := id
      ~> encodeCalendarContent content

encodeCalendarContent :: CalendarItemContent -> Json
encodeCalendarContent { itemType, title, windowStart, windowEnd, status, sourceItemId, actualDurationMinutes, category, recurrenceRule, recurrenceExceptionDates } =
  withRecurrence $ withCategory $ withDuration $ withSourceItem $
    "type" := itemType
      ~> "titre" := title
      ~> "fenetre_debut" := windowStart
      ~> "fenetre_fin" := windowEnd
      ~> "statut" := status
      ~> jsonEmptyObject
  where
  withSourceItem base =
    case sourceItemId of
      Just sourceId -> "source_item_id" := sourceId ~> base
      Nothing -> base
  withDuration base =
    case actualDurationMinutes of
      Just minutes -> "duree_reelle_minutes" := minutes ~> base
      Nothing -> base
  withCategory base =
    case category of
      Just value -> "categorie" := value ~> base
      Nothing -> base
  withRecurrence base =
    case recurrenceRule of
      Just rule ->
        "recurrence_rule" := encodeJson rule
          ~> "recurrence_exception_dates" := recurrenceExceptionDates
          ~> base
      Nothing -> base

type State =
  { items :: Array CalendarItem
  , draft :: IntentionDraft
  , validationError :: Maybe ValidationError
  , showConflictsOnly :: Boolean
  , conflictResolution :: Maybe ConflictResolution
  , offlineMode :: Boolean
  , pendingSync :: Array CalendarItem
  , syncConflict :: Maybe (Array CalendarItem)
  , validationPanel :: Maybe ValidationPanel
  , sortMode :: SortMode
  , draggingId :: Maybe String
  }

data Action
  = Initialize
  | DraftTitleChanged String
  | DraftStartChanged String
  | DraftEndChanged String
  | DraftCategoryChanged String
  | SubmitIntention
  | PlanifyFrom String CalendarItemContent
  | ToggleConflictFilter
  | ToggleOffline
  | SortChanged String
  | DragStart String
  | DragOver String DragEvent
  | DropOn String
  | DragEnd
  | OpenValidation String CalendarItemContent
  | ValidationMinutesChanged String
  | ConfirmValidation
  | CancelValidation
  | OpenConflictResolution (Array String)
  | ChooseResolutionStrategy ResolutionStrategy
  | ConfirmResolution
  | CancelResolution
  | ResolveSyncKeepLocal
  | ResolveSyncDiscardLocal

component :: forall q i. H.Component q i NoOutput Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                     , initialize = pure Initialize
                                     }
    }

initialState :: forall i. i -> State
initialState = const
  { items: []
  , draft: emptyDraft
  , validationError: Nothing
  , showConflictsOnly: false
  , conflictResolution: Nothing
  , offlineMode: false
  , pendingSync: []
  , syncConflict: Nothing
  , validationPanel: Nothing
  , sortMode: SortByTime
  , draggingId: Nothing
  }

handleAction :: Action -> AgendaAppM Unit
handleAction action = handleError $
  case action of
    Initialize -> refreshItems
    DraftTitleChanged title ->
      lift $ modify_ \st -> st { draft = st.draft { title = title }, validationError = Nothing }
    DraftStartChanged windowStart ->
      lift $ modify_ \st -> st { draft = st.draft { windowStart = windowStart }, validationError = Nothing }
    DraftEndChanged windowEnd ->
      lift $ modify_ \st -> st { draft = st.draft { windowEnd = windowEnd }, validationError = Nothing }
    DraftCategoryChanged category ->
      lift $ modify_ \st -> st { draft = st.draft { category = category }, validationError = Nothing }
    SubmitIntention -> do
      st <- get
      case validateIntention st.draft of
        Left err -> lift $ modify_ _ { validationError = Just err }
        Right validDraft -> do
          let item = toNewIntention validDraft
          if st.offlineMode then do
            let
              result = applyOfflineMutation true item st.items st.pendingSync
            lift $ modify_ _ { items = result.items
                            , pendingSync = result.pending
                            , draft = emptyDraft
                            , validationError = Nothing
                            }
          else do
            _ <- createItem item
            lift $ modify_ _ { draft = emptyDraft, validationError = Nothing }
            refreshItems
    PlanifyFrom sourceId content -> do
      st <- get
      let item = toScheduledBlock sourceId content
      if st.offlineMode then do
        let
          result = applyOfflineMutation true item st.items st.pendingSync
        lift $ modify_ _ { items = result.items, pendingSync = result.pending }
      else do
        _ <- createItem item
        refreshItems
    ToggleConflictFilter ->
      lift $ modify_ \st -> st { showConflictsOnly = not st.showConflictsOnly }
    ToggleOffline -> do
      st <- get
      if st.offlineMode then do
        lift $ modify_ _ { offlineMode = false }
        syncPending
      else lift $ modify_ _ { offlineMode = true }
    SortChanged raw ->
      lift $ modify_ \st -> st { sortMode = parseSortMode raw }
    DragStart itemId ->
      lift $ modify_ _ { draggingId = Just itemId }
    DragOver _ ev ->
      liftEffect $ preventDefault (toEvent ev)
    DropOn targetId -> do
      st <- get
      case st.draggingId of
        Nothing -> pure unit
        Just draggingId -> do
          let reordered = moveItemBefore draggingId targetId st.items
          lift $ modify_ _ { items = reordered, draggingId = Nothing }
    DragEnd ->
      lift $ modify_ _ { draggingId = Nothing }
    OpenValidation itemId content -> do
      suggested <- liftEffect $ suggestDurationMinutes content.windowStart
      lift $ modify_ _ { validationPanel = Just { itemId, proposedMinutes: suggested, inputValue: "" } }
    ValidationMinutesChanged raw ->
      lift $ modify_ \st -> st { validationPanel = st.validationPanel <#> \panel -> panel { inputValue = raw } }
    ConfirmValidation -> do
      st <- get
      case st.validationPanel of
        Nothing -> pure unit
        Just panel -> do
          let duration = parsePositiveInt panel.inputValue <|> panel.proposedMinutes
          case duration of
            Nothing -> pure unit
            Just minutes -> do
              _ <- validateItem panel.itemId minutes
              lift $ modify_ _ { validationPanel = Nothing }
              refreshItems
    CancelValidation ->
      lift $ modify_ _ { validationPanel = Nothing }
    OpenConflictResolution groupIds ->
      lift $ modify_ \st -> st { conflictResolution = Just { groupIds, pendingStrategy: Nothing } }
    ChooseResolutionStrategy strategy ->
      lift $ modify_ \st -> st { conflictResolution = st.conflictResolution <#> \res -> res { pendingStrategy = Just strategy } }
    ConfirmResolution ->
      lift $ modify_ \st -> st { conflictResolution = Nothing }
    CancelResolution ->
      lift $ modify_ \st -> st { conflictResolution = Nothing }
    ResolveSyncKeepLocal ->
      lift $ modify_ \st -> st { syncConflict = Nothing, offlineMode = true }
    ResolveSyncDiscardLocal -> do
      lift $ modify_ \st -> st { syncConflict = Nothing, pendingSync = [] }
      refreshItems

handleError :: ErrorAgendaAppM Unit -> AgendaAppM Unit
handleError m = do
  res <- runExceptT m
  either logShow pure res

refreshItems :: ErrorAgendaAppM Unit
refreshItems = do
  jsonResponse <- withExceptT toFatalError $ ExceptT $ liftAff $ Affjax.get json "/api/v1/calendar-items"
  items <- (_.body >>> decodeJson >>> lmap toFatalError >>> pure >>> ExceptT) jsonResponse
  lift $ modify_ \st -> st { items = items }

createItem :: CalendarItem -> ErrorAgendaAppM (Response Json)
createItem item = withExceptT toFatalError $ ExceptT $ liftAff $ post json "/api/v1/calendar-items" (Just $ Json $ encodeJson item)

validateItem :: String -> Int -> ErrorAgendaAppM (Response Json)
validateItem itemId minutes =
  withExceptT toFatalError $ ExceptT $ liftAff $
    post json ("/api/v1/calendar-items/" <> itemId <> "/validate")
      (Just $ Json $ "duree_reelle_minutes" := minutes ~> jsonEmptyObject)

statusOk :: forall a. Response a -> Boolean
statusOk r = unwrap r.status >= 200 && unwrap r.status < 300

syncPending :: ErrorAgendaAppM Unit
syncPending = do
  st <- get
  if null st.pendingSync then refreshItems
  else do
    ok <- foldM
      (\acc item ->
        if not acc then pure false
        else do
          resp <- createItem item
          pure (statusOk resp)
      )
      true
      st.pendingSync
    if ok then do
      lift $ modify_ _ { pendingSync = [], syncConflict = Nothing }
      refreshItems
    else lift $ modify_ _ { syncConflict = Just st.pendingSync }

render :: forall m. State -> H.ComponentHTML Action () m
render { items, draft, validationError, showConflictsOnly, conflictResolution, offlineMode, syncConflict, validationPanel, sortMode } =
  let
    conflictIds = detectConflictIds items
    conflictGroups = detectConflictGroups items
    itemsToShow =
      if showConflictsOnly
        then filter (isConflict conflictIds) items
        else items
    sortedItems = sortItems sortMode conflictIds itemsToShow
  in
  div [ class_ "entity-page agenda-page" ]
    [ section [ class_ "agenda-header" ]
        [ h2 [ class_ "agenda-title" ] [ text "Vue Jour" ]
        , div [ class_ "agenda-subtitle" ] [ text "Capture rapide des intentions a planifier." ]
        , button
            [ class_ $ "btn btn-sm agenda-filter" <> if showConflictsOnly then " btn-outline-primary" else " btn-outline-secondary"
            , onClick (const ToggleConflictFilter)
            ]
            [ text "Filtrer: en conflit" ]
        , renderOfflineToggle offlineMode
        , renderSortPicker sortMode
        , renderConflictActions conflictGroups
        ]
    , renderForm draft validationError
    , if (null sortedItems) then emptyAgenda else agendaList conflictIds sortedItems
    , maybe (text "") (renderConflictResolution items) conflictResolution
    , maybe (text "") renderSyncConflict syncConflict
    , maybe (text "") renderValidationPanel validationPanel
    ]

renderForm :: forall w. IntentionDraft -> Maybe ValidationError -> HTML w Action
renderForm draft validationError =
  section [ class_ "agenda-form" ]
    [ input
        [ class_ "form-control agenda-input"
        , placeholder "Titre de l'intention"
        , onValueChange DraftTitleChanged
        , value draft.title
        ]
    , div [ class_ "agenda-time-row" ]
        [ input
            [ class_ "form-control agenda-input"
            , type_ InputDatetimeLocal
            , placeholder "Debut"
            , onValueChange DraftStartChanged
            , value draft.windowStart
            ]
        , input
            [ class_ "form-control agenda-input"
            , type_ InputDatetimeLocal
            , placeholder "Fin"
            , onValueChange DraftEndChanged
            , value draft.windowEnd
            ]
        ]
    , input
        [ class_ "form-control agenda-input"
        , placeholder "Categorie (optionnelle)"
        , onValueChange DraftCategoryChanged
        , value draft.category
        ]
    , maybe (text "") renderValidationError validationError
    , button [ class_ "btn btn-primary agenda-submit", onClick (const SubmitIntention) ] [ text "Creer l'intention" ]
    ]

renderValidationError :: forall w. ValidationError -> HTML w Action
renderValidationError err =
  div [ class_ "agenda-error" ]
    [ text $ case err of
        TitleEmpty -> "Le titre est obligatoire."
        WindowStartInvalid -> "La date de debut est invalide."
        WindowEndInvalid -> "La date de fin est invalide."
        WindowOrderInvalid -> "La fin doit etre apres le debut."
    ]

emptyAgenda :: forall w i. HTML w i
emptyAgenda =
  div [ class_ "row entity-empty agenda-empty" ]
    [ div [ class_ "entity-empty-title" ] [ text "Aucune intention aujourd'hui" ]
    , div [ class_ "entity-empty-subtitle" ] [ text "Ajoutez une intention pour demarrer votre journee." ]
    ]

agendaList :: forall w. Array String -> Array CalendarItem -> HTML w Action
agendaList conflictIds items =
  ul [ class_ "list-group entity-list agenda-list" ] (mapWithIndex (renderItem conflictIds) items)

renderItem :: forall w. Array String -> Int -> CalendarItem -> HTML w Action
renderItem conflictIds _ item =
  let
    content = calendarItemContent item
    conflictClass = if isConflict conflictIds item then " agenda-card--conflict" else ""
    dragProps = dragHandlers item
  in
    li ([ class_ $ "row list-group-item entity-card agenda-card" <> conflictClass ] <> dragProps)
      [ div [ class_ "col entity-card-body" ]
          [ div [ class_ "agenda-card-time" ] [ text (timeLabel content.windowStart) ]
          , div [ class_ "agenda-card-title" ] [ text content.title ]
          , div [ class_ "agenda-card-window" ]
              [ text $ content.windowStart <> " → " <> content.windowEnd ]
          , renderCategory content.category
          , renderValidationAction item content
          , renderPlanifyAction item content
          ]
      ]

renderPlanifyAction :: forall w. CalendarItem -> CalendarItemContent -> HTML w Action
renderPlanifyAction (ServerCalendarItem { id, content }) _ | content.itemType == Intention =
  button [ class_ "btn btn-sm btn-outline-primary agenda-planify", onClick (const $ PlanifyFrom id content) ]
    [ text "Planifier" ]
renderPlanifyAction _ _ = text ""

renderValidationAction :: forall w. CalendarItem -> CalendarItemContent -> HTML w Action
renderValidationAction (ServerCalendarItem { id, content }) _ | content.status /= Fait =
  button [ class_ "btn btn-sm btn-outline-success agenda-validate", onClick (const $ OpenValidation id content) ]
    [ text "Valider" ]
renderValidationAction _ _ = text ""

renderCategory :: forall w. Maybe String -> HTML w Action
renderCategory category =
  case category of
    Nothing -> text ""
    Just value -> div [ class_ "agenda-card-category" ] [ text value ]

timeLabel :: String -> String
timeLabel raw =
  if String.length raw >= 16 then String.slice 11 16 raw else raw

dragHandlers ::
  forall r.
  CalendarItem ->
  Array
    (IProp
       ( draggable :: Boolean
       , onDragStart :: DragEvent
       , onDragOver :: DragEvent
       , onDrop :: DragEvent
       , onDragEnd :: DragEvent
       | r
       )
       Action
    )
dragHandlers (ServerCalendarItem { id }) =
  [ draggable true
  , onDragStart (const $ DragStart id)
  , onDragOver (\ev -> DragOver id ev)
  , onDrop (const $ DropOn id)
  , onDragEnd (const DragEnd)
  ]
dragHandlers _ = []

isConflict :: Array String -> CalendarItem -> Boolean
isConflict conflictIds (ServerCalendarItem { id }) = elem id conflictIds
isConflict _ _ = false

type ConflictBlock =
  { id :: String
  , start :: String
  , end :: String
  }

type ConflictResolution =
  { groupIds :: Array String
  , pendingStrategy :: Maybe ResolutionStrategy
  }

type ValidationPanel =
  { itemId :: String
  , proposedMinutes :: Maybe Int
  , inputValue :: String
  }

data ResolutionStrategy
  = StrategyShift30
  | StrategySwap

derive instance resolutionStrategyGeneric :: Generic ResolutionStrategy _
derive instance resolutionStrategyEq :: Eq ResolutionStrategy
instance resolutionStrategyShow :: Show ResolutionStrategy where
  show = genericShow

data SortMode
  = SortByTime
  | SortByStatus
  | SortByCategory
  | SortByConflict

derive instance sortModeGeneric :: Generic SortMode _
derive instance sortModeEq :: Eq SortMode
instance sortModeShow :: Show SortMode where
  show = genericShow

type OfflineMutationResult =
  { items :: Array CalendarItem
  , pending :: Array CalendarItem
  }

applyOfflineMutation :: Boolean -> CalendarItem -> Array CalendarItem -> Array CalendarItem -> OfflineMutationResult
applyOfflineMutation offline item items pending =
  if offline
    then { items: items <> [ item ], pending: pending <> [ item ] }
    else { items, pending }

durationMinutesBetween :: String -> String -> Maybe Int
durationMinutesBetween start end = do
  startDt <- parseDateTimeLocal start
  endDt <- parseDateTimeLocal end
  let Minutes n = diff endDt startDt
      minutes = Int.floor n
  pure $ max 1 minutes

suggestDurationMinutes :: String -> Effect (Maybe Int)
suggestDurationMinutes start = do
  now <- nowDateTime
  pure $ durationMinutesBetweenDateTime start now

durationMinutesBetweenDateTime :: String -> DateTime -> Maybe Int
durationMinutesBetweenDateTime start now = do
  startDt <- parseDateTimeLocal start
  let Minutes n = diff now startDt
      minutes = Int.floor n
  pure $ max 1 minutes

parseDateTimeLocal :: String -> Maybe DateTime
parseDateTimeLocal raw = do
  year <- parseInt (slice 0 4)
  monthNum <- parseInt (slice 5 7)
  dayNum <- parseInt (slice 8 10)
  hourNum <- parseInt (slice 11 13)
  minuteNum <- parseInt (slice 14 16)
  month <- toEnum monthNum
  day <- toEnum dayNum
  hour <- toEnum hourNum
  minute <- toEnum minuteNum
  yearEnum <- toEnum year
  date <- exactDate yearEnum month day
  second <- toEnum 0
  millisecond <- toEnum 0
  pure $ DateTime date (Time hour minute second millisecond)
  where
  slice start end = String.slice start end raw
  parseInt str = Int.fromString str

parsePositiveInt :: String -> Maybe Int
parsePositiveInt raw =
  Int.fromString (StringCommon.trim raw) >>= \val ->
    if val > 0 then Just val else Nothing

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

instantiateRoutine :: RoutineTemplate -> RoutineInstance
instantiateRoutine template =
  let
    steps = map toInstance template.steps
    withDeps = applyDependencies template.steps steps
  in
    { templateId: template.id
    , steps: withDeps
    }
  where
  toInstance step =
    { id: step.id
    , title: step.title
    , windowStart: step.windowStart
    , windowEnd: step.windowEnd
    , sourceStepId: step.id
    }

applyDependencies :: Array RoutineTemplateStep -> Array RoutineInstanceStep -> Array RoutineInstanceStep
applyDependencies templateSteps instanceSteps =
  map (applyDependency templateSteps instanceSteps) instanceSteps

applyDependency :: Array RoutineTemplateStep -> Array RoutineInstanceStep -> RoutineInstanceStep -> RoutineInstanceStep
applyDependency templateSteps instanceSteps step =
  case find (\templateStep -> templateStep.id == step.sourceStepId) templateSteps of
    Nothing -> step
    Just templateStep ->
      case templateStep.dependsOn of
        Nothing -> step
        Just dependency ->
          case dependency of
            StartAfterEnd { stepId, offsetMinutes } ->
              updateFromBase stepId offsetMinutes _.windowEnd
            StartBeforeStart { stepId, offsetMinutes } ->
              updateFromBase stepId (-offsetMinutes) _.windowStart
  where
  updateFromBase baseId offset selectBase =
    case find (\candidate -> candidate.id == baseId) instanceSteps of
      Nothing -> step
      Just base ->
        let
          duration = durationMinutesBetween step.windowStart step.windowEnd
          newStart = shiftMinutes offset (selectBase base)
          newEnd = newStart >>= \start -> duration >>= \mins -> shiftMinutes mins start
        in
          case { start: newStart, end: newEnd } of
            { start: Just start, end: Just end } -> step { windowStart = start, windowEnd = end }
            _ -> step

shiftMinutes :: Int -> String -> Maybe String
shiftMinutes offset start = do
  dt <- parseDateTimeLocal start
  newDt <- adjust (Minutes (Int.toNumber offset)) dt
  pure $ formatDateTimeLocal newDt

nextOccurrence :: RecurrenceRule -> DateTime -> Maybe DateTime
nextOccurrence rule dt =
  case rule of
    RecurrenceDaily -> addDays 1 dt
    RecurrenceWeekly -> addDays 7 dt
    RecurrenceEveryXDays interval -> addDays interval dt
    RecurrenceMonthly -> Just (addMonths 1 dt)
    RecurrenceYearly -> Just (addMonths 12 dt)

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

formatDateTimeLocal :: DateTime -> String
formatDateTimeLocal dt =
  formatDate dt <> "T" <> formatTime (time dt)

formatTime :: Time -> String
formatTime t =
  let
    h = pad2 (fromEnum (hour t))
    m = pad2 (fromEnum (minute t))
  in
    h <> ":" <> m

pad2 :: Int -> String
pad2 n =
  let raw = Int.toStringAs Int.decimal n
  in if String.length raw == 1 then "0" <> raw else raw

detectConflictIds :: Array CalendarItem -> Array String
detectConflictIds items =
  nub $ go (mapMaybe toConflictBlock items) []
  where
  toConflictBlock :: CalendarItem -> Maybe ConflictBlock
  toConflictBlock (ServerCalendarItem { id, content }) | content.itemType == ScheduledBlock =
    Just { id, start: content.windowStart, end: content.windowEnd }
  toConflictBlock _ = Nothing

  overlaps a b = a.start < b.end && b.start < a.end

  go blocks acc =
    case uncons blocks of
      Nothing -> acc
      Just { head: current, tail: rest } ->
        let
          acc' =
            foldl
              (\currentAcc other ->
                if overlaps current other
                  then currentAcc <> [ current.id, other.id ]
                  else currentAcc
              )
              acc
              rest
        in
          go rest acc'

detectConflictGroups :: Array CalendarItem -> Array (Array String)
detectConflictGroups items =
  filter (\group -> length group > 1) $ components allIds []
  where
  blocks = mapMaybe toConflictBlock items
  allIds = map _.id blocks

  toConflictBlock :: CalendarItem -> Maybe ConflictBlock
  toConflictBlock (ServerCalendarItem { id, content }) | content.itemType == ScheduledBlock =
    Just { id, start: content.windowStart, end: content.windowEnd }
  toConflictBlock _ = Nothing

  components ids visited =
    case uncons ids of
      Nothing -> []
      Just { head: current, tail } ->
        if elem current visited then components tail visited
        else
          let
            group = bfs [ current ] []
            newVisited = visited <> group
          in
            [ group ] <> components tail newVisited

  bfs queue visited =
    case uncons queue of
      Nothing -> visited
      Just { head: current, tail } ->
        if elem current visited then bfs tail visited
        else
          let
            next = neighbors current
          in
            bfs (tail <> next) (visited <> [ current ])

  neighbors id =
    case find (\block -> block.id == id) blocks of
      Nothing -> []
      Just current ->
        map _.id $ filter (\block -> block.id /= id && overlaps current block) blocks

  overlaps a b = a.start < b.end && b.start < a.end

renderConflictActions :: forall w. Array (Array String) -> HTML w Action
renderConflictActions conflictGroups =
  if null conflictGroups then text ""
  else
    div [ class_ "agenda-conflict-actions" ]
      [ button
          [ class_ "btn btn-sm btn-outline-danger agenda-conflict-button"
          , onClick (const $ OpenConflictResolution (headOrEmpty conflictGroups))
          ]
          [ text "Resoudre un conflit" ]
      ]
  where
  headOrEmpty groups =
    case uncons groups of
      Just { head } -> head
      Nothing -> []

renderOfflineToggle :: forall w. Boolean -> HTML w Action
renderOfflineToggle offlineMode =
  div [ class_ "agenda-offline-toggle" ]
    [ button
        [ class_ $ "btn btn-sm " <> if offlineMode then "btn-outline-warning" else "btn-outline-secondary"
        , onClick (const ToggleOffline)
        ]
        [ text $ if offlineMode then "Mode hors ligne actif" else "Passer hors ligne" ]
    ]

renderSortPicker :: forall w. SortMode -> HTML w Action
renderSortPicker sortMode =
  div [ class_ "agenda-sort" ]
    [ text "Trier:"
    , select
        [ class_ "form-select agenda-sort-select"
        , onValueChange SortChanged
        , value (sortModeValue sortMode)
        ]
        [ option [ value "time" ] [ text "Horaire" ]
        , option [ value "status" ] [ text "Statut" ]
        , option [ value "category" ] [ text "Categorie" ]
        , option [ value "conflict" ] [ text "Conflit" ]
        ]
    ]

sortModeValue :: SortMode -> String
sortModeValue SortByTime = "time"
sortModeValue SortByStatus = "status"
sortModeValue SortByCategory = "category"
sortModeValue SortByConflict = "conflict"

parseSortMode :: String -> SortMode
parseSortMode raw =
  case raw of
    "status" -> SortByStatus
    "category" -> SortByCategory
    "conflict" -> SortByConflict
    _ -> SortByTime

sortItems :: SortMode -> Array String -> Array CalendarItem -> Array CalendarItem
sortItems mode conflictIds items =
  case mode of
    SortByStatus -> sortBy compareStatus items
    SortByCategory -> sortBy compareCategory items
    SortByConflict -> sortBy (compareConflict conflictIds) items
    SortByTime -> sortBy compareTime items
  where
  compareTime a b = compare (calendarItemContent a).windowStart (calendarItemContent b).windowStart

  compareStatus a b = compare (statusRank (calendarItemContent a).status) (statusRank (calendarItemContent b).status)

  compareCategory a b = compare (categoryKey (calendarItemContent a).category) (categoryKey (calendarItemContent b).category)

  compareConflict ids a b = compare (conflictRank ids a) (conflictRank ids b)

  conflictRank ids item = if isConflict ids item then 0 else 1

  statusRank Todo = 0
  statusRank EnCours = 1
  statusRank Fait = 2
  statusRank Annule = 3

  categoryKey Nothing = "~~~"
  categoryKey (Just value) = value

moveItemBefore :: String -> String -> Array CalendarItem -> Array CalendarItem
moveItemBefore dragId targetId items =
  case { from: indexOf dragId items, to: indexOf targetId items } of
    { from: Just fromIdx, to: Just toIdx } ->
      let
        without = deleteAtIndex fromIdx items
        adjustedTo = if fromIdx < toIdx then toIdx - 1 else toIdx
        draggedItem = index items fromIdx
      in
        insertAtIndex adjustedTo draggedItem without
    _ -> items
  where
  indexOf id = findIndex (matchesId id)
  matchesId id (ServerCalendarItem { id: candidate }) = id == candidate
  matchesId _ _ = false

  findIndex predicate arr =
    case uncons arr of
      Nothing -> Nothing
      Just { head, tail } ->
        if predicate head then Just 0
        else map (_ + 1) (findIndex predicate tail)

  deleteAtIndex idx arr =
    case uncons arr of
      Nothing -> []
      Just { head, tail } ->
        if idx == 0 then tail
        else [ head ] <> deleteAtIndex (idx - 1) tail

  insertAtIndex idx maybeItem arr =
    case maybeItem of
      Nothing -> arr
      Just item ->
        if idx <= 0 then [ item ] <> arr
        else case uncons arr of
          Nothing -> [ item ]
          Just { head, tail } -> [ head ] <> insertAtIndex (idx - 1) (Just item) tail

renderSyncConflict :: forall w. Array CalendarItem -> HTML w Action
renderSyncConflict pending =
  div [ class_ "agenda-sync-conflict" ]
    [ div [ class_ "agenda-conflict-title" ] [ text "Conflit de synchronisation" ]
    , div [ class_ "agenda-conflict-subtitle" ]
        [ text "Choisissez comment resoudre la synchronisation des changements locaux." ]
    , ul [ class_ "agenda-conflict-list" ] (map (renderConflictItem pending) pendingIds)
    , div [ class_ "agenda-conflict-confirmation-actions" ]
        [ button [ class_ "btn btn-sm btn-danger", onClick (const ResolveSyncDiscardLocal) ] [ text "Abandonner local" ]
        , button [ class_ "btn btn-sm btn-outline-secondary", onClick (const ResolveSyncKeepLocal) ] [ text "Conserver local" ]
        ]
    ]
  where
  pendingIds = mapMaybe extractId pending
  extractId (ServerCalendarItem { id }) = Just id
  extractId _ = Nothing

renderValidationPanel :: forall w. ValidationPanel -> HTML w Action
renderValidationPanel panel =
  div [ class_ "agenda-validation-panel" ]
    [ div [ class_ "agenda-conflict-title" ] [ text "Valider la tache" ]
    , div [ class_ "agenda-conflict-subtitle" ]
        [ text "Saisissez la duree reelle (minutes) ou acceptez la proposition." ]
    , maybe (text "") (\minutes -> div [ class_ "agenda-validation-proposal" ] [ text $ "Proposition: " <> show minutes <> " min" ]) panel.proposedMinutes
    , input
        [ class_ "form-control agenda-input"
        , placeholder "Duree reelle (minutes)"
        , onValueChange ValidationMinutesChanged
        , value panel.inputValue
        ]
    , div [ class_ "agenda-conflict-confirmation-actions" ]
        [ button [ class_ "btn btn-sm btn-success", onClick (const ConfirmValidation) ] [ text "Confirmer" ]
        , button [ class_ "btn btn-sm btn-outline-secondary", onClick (const CancelValidation) ] [ text "Annuler" ]
        ]
    ]

renderConflictResolution :: forall w. Array CalendarItem -> ConflictResolution -> HTML w Action
renderConflictResolution items resolution =
  div [ class_ "agenda-conflict-panel" ]
    [ div [ class_ "agenda-conflict-title" ] [ text "Resolution de conflit" ]
    , div [ class_ "agenda-conflict-subtitle" ] [ text "Choisissez une strategie puis confirmez." ]
    , ul [ class_ "agenda-conflict-list" ] (map (renderConflictItem items) resolution.groupIds)
    , div [ class_ "agenda-conflict-strategies" ]
        [ button
            [ class_ "btn btn-sm btn-outline-primary"
            , onClick (const $ ChooseResolutionStrategy StrategyShift30)
            ]
            [ text "Decaler de 30 min" ]
        , button
            [ class_ "btn btn-sm btn-outline-primary"
            , onClick (const $ ChooseResolutionStrategy StrategySwap)
            ]
            [ text "Echanger" ]
        ]
    , renderConfirmation resolution.pendingStrategy
    , button [ class_ "btn btn-sm btn-outline-secondary", onClick (const CancelResolution) ] [ text "Fermer" ]
    ]

renderConflictItem :: forall w. Array CalendarItem -> String -> HTML w Action
renderConflictItem items itemId =
  case find (matchId itemId) items of
    Just item ->
      let
        content = calendarItemContent item
      in
        li [ class_ "agenda-conflict-item" ]
          [ div [ class_ "agenda-conflict-item-title" ] [ text content.title ]
          , div [ class_ "agenda-conflict-item-window" ]
              [ text $ content.windowStart <> " → " <> content.windowEnd ]
          ]
    Nothing -> text ""
  where
  matchId id (ServerCalendarItem { id: candidate }) = id == candidate
  matchId _ _ = false

renderConfirmation :: forall w. Maybe ResolutionStrategy -> HTML w Action
renderConfirmation pending =
  case pending of
    Nothing -> text ""
    Just strategy ->
      div [ class_ "agenda-conflict-confirmation" ]
        [ div [ class_ "agenda-conflict-confirmation-text" ]
            [ text $ "Confirmer la strategie: " <> show strategy <> " ?" ]
        , div [ class_ "agenda-conflict-confirmation-actions" ]
            [ button [ class_ "btn btn-sm btn-danger", onClick (const ConfirmResolution) ] [ text "Confirmer" ]
            , button [ class_ "btn btn-sm btn-outline-secondary", onClick (const CancelResolution) ] [ text "Annuler" ]
            ]
        ]

calendarItemContent :: CalendarItem -> CalendarItemContent
calendarItemContent (NewCalendarItem { content }) = content
calendarItemContent (ServerCalendarItem { content }) = content

data FatalError
  = DecodeError JsonDecodeError
  | NetworkError Error
  | CustomFatalError String

instance fatalErrorShowInstance :: Show FatalError where
  show (DecodeError err) = "DecodeError: " <> show err
  show (NetworkError err) = "NetworkError: " <> printError err
  show (CustomFatalError err) = "CustomError: " <> err

class ToFatalError a where
  toFatalError :: a -> FatalError

instance jsonDecodeErrorToFatalErrorInstance :: ToFatalError JsonDecodeError where
  toFatalError = DecodeError

instance affjaxErrorToFatalErrorInstance :: ToFatalError Error where
  toFatalError = NetworkError
