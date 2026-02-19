module Agenda
  ( component
  , CalendarItem(..)
  , CalendarItemContent
  , IntentionDraft
  , ItemStatus(..)
  , ItemType(..)
  , ValidationError(..)
  , toNewIntention
  , validateIntention
  ) where

import Prelude hiding (div)

import Affjax (Error, printError)
import Affjax.RequestBody (RequestBody(..))
import Affjax.ResponseFormat (json)
import Affjax.Web (Response, post)
import Affjax.Web (get) as Affjax
import Control.Monad.Except (ExceptT(..), runExceptT, withExceptT)
import Control.Monad.RWS (get, modify_)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut.Core (Json, jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Array (mapWithIndex, null)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Foldable (all)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), maybe)
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits as String
import Data.String.Common as StringCommon
import DOM.HTML.Indexed.InputType (InputType(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (logShow)
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, mkComponent, mkEval) as H
import Halogen.HTML (HTML, button, div, h2, input, li, section, text, ul)
import Halogen.HTML.Events (onClick, onValueChange)
import Halogen.HTML.Properties (placeholder, type_, value)
import Utils (class_)

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

type CalendarItemContent =
  { itemType :: ItemType
  , title :: String
  , windowStart :: String
  , windowEnd :: String
  , status :: ItemStatus
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
  }

toNewIntention :: IntentionDraft -> CalendarItem
toNewIntention { title, windowStart, windowEnd } =
  NewCalendarItem
    { content:
        { itemType: Intention
        , title
        , windowStart
        , windowEnd
        , status: Todo
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

instance calendarItemDecodeJson :: DecodeJson CalendarItem where
  decodeJson json = do
    obj <- decodeJson json
    itemType <- obj .: "type"
    title <- obj .: "titre"
    windowStart <- obj .: "fenetre_debut"
    windowEnd <- obj .: "fenetre_fin"
    status <- obj .: "statut"
    let content = { itemType, title, windowStart, windowEnd, status }
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
encodeCalendarContent { itemType, title, windowStart, windowEnd, status } =
  "type" := itemType
    ~> "titre" := title
    ~> "fenetre_debut" := windowStart
    ~> "fenetre_fin" := windowEnd
    ~> "statut" := status
    ~> jsonEmptyObject

type State =
  { items :: Array CalendarItem
  , draft :: IntentionDraft
  , validationError :: Maybe ValidationError
  }

data Action
  = Initialize
  | DraftTitleChanged String
  | DraftStartChanged String
  | DraftEndChanged String
  | SubmitIntention

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
    SubmitIntention -> do
      st <- get
      case validateIntention st.draft of
        Left err -> lift $ modify_ _ { validationError = Just err }
        Right validDraft -> do
          _ <- createItem (toNewIntention validDraft)
          lift $ modify_ _ { draft = emptyDraft, validationError = Nothing }
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

render :: forall m. State -> H.ComponentHTML Action () m
render { items, draft, validationError } =
  div [ class_ "entity-page agenda-page" ]
    [ section [ class_ "agenda-header" ]
        [ h2 [ class_ "agenda-title" ] [ text "Vue Jour" ]
        , div [ class_ "agenda-subtitle" ] [ text "Capture rapide des intentions a planifier." ]
        ]
    , renderForm draft validationError
    , if (null items) then emptyAgenda else agendaList items
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

agendaList :: forall w. Array CalendarItem -> HTML w Action
agendaList items =
  ul [ class_ "list-group entity-list agenda-list" ] (mapWithIndex renderItem items)

renderItem :: forall w. Int -> CalendarItem -> HTML w Action
renderItem _ item =
  let
    content = calendarItemContent item
  in
    li [ class_ "row list-group-item entity-card agenda-card" ]
      [ div [ class_ "col entity-card-body" ]
          [ div [ class_ "agenda-card-title" ] [ text content.title ]
          , div [ class_ "agenda-card-window" ]
              [ text $ content.windowStart <> " → " <> content.windowEnd ]
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
