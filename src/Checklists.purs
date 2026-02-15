module Checklists (component, Checklist(..), ChecklistContent, ChecklistItem(..), StorageId, removeChecklistItem) where

import Prelude hiding (div)

import Affjax (Error, printError)
import Affjax.RequestBody (RequestBody(..))
import Affjax.ResponseFormat (json)
import Affjax.Web (Response, delete, post, put)
import Affjax.Web (get) as Affjax
import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT(..), runExceptT, withExceptT)
import Control.Monad.RWS (get, gets, modify_)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut.Core (Json, jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError, decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Array (deleteAt, index, length, mapWithIndex, null, snoc)
import Data.Bifunctor (lmap)
import Data.Either (Either, either)
import Data.Generic.Rep (class Generic)
import Data.Int (floor, toNumber)
import Data.Lens (Lens', Traversal', lens, lens', (.~), (^.), (^?))
import Data.Lens.Index (ix)
import Data.Maybe (Maybe, maybe)
import Data.Newtype (unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import Foreign.Object (Object)
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, getRef, mkComponent, mkEval, put) as H
import Halogen.HTML (HTML, button, div, h2, header, i, input, li, section, span, text, ul)
import Halogen.HTML.Events (onBlur, onClick, onValueChange)
import Halogen.HTML.Properties (ButtonType(..), ref, type_, value)
import Utils (class_)
import Web.DOM (Element)
import Web.DOM.Element (getBoundingClientRect, getElementsByClassName)
import Web.DOM.HTMLCollection (item)
import Web.HTML (window)
import Web.HTML.HTMLElement (focus, fromElement) as HTMLElement
import Web.HTML.HTMLInputElement (fromElement) as InputElement
import Web.HTML.HTMLInputElement (select)
import Web.HTML.Window (innerHeight, scroll)

type NoOutput = Void
type ChecklistAppM = H.HalogenM State Action () NoOutput Aff
type ErrorChecklistAppM = ExceptT FatalError ChecklistAppM
type State = { checklists :: Array Checklist
             , editingState :: EditingState
             }

data EditingState = None
                  | EditingChecklistName Int
                  | EditingChecklistContent Int Int

data Checklist = NewChecklist { content   :: ChecklistContent }
          | ServerChecklist { content   :: ChecklistContent
                       , storageId :: StorageId }

derive instance checklistGenericInstance :: Generic Checklist _
derive instance checklistEqInstance :: Eq Checklist
instance checklistShowInstance :: Show Checklist where
  show = genericShow

derive newtype instance checklistItemEqInstance :: Eq ChecklistItem
derive newtype instance checklistItemShowInstance :: Show ChecklistItem

_content  :: Lens' Checklist ChecklistContent
_content = lens' $ (\checklist -> Tuple (getContent checklist) (setContent checklist))


getContent :: Checklist -> ChecklistContent
getContent (NewChecklist n) = n.content
getContent (ServerChecklist n) = n.content

setContent :: Checklist -> ChecklistContent -> Checklist
setContent (NewChecklist _) newContent = NewChecklist { content: newContent }
setContent (ServerChecklist n) newContent = ServerChecklist (n { content = newContent })

_name :: Lens' Checklist String
_name = _content <<< (lens _.name $ _ { name = _ })

_checklistItems :: Lens' Checklist (Array ChecklistItem)
_checklistItems = _content <<< (lens _.items $ _ { items = _ })

_label :: Lens' ChecklistItem String
_label = lens' $ (\(ChecklistItem { label, checked }) -> Tuple label (\newLabel -> ChecklistItem { label: newLabel, checked: checked }))

instance checklistDecodeJsonInstance :: DecodeJson Checklist where
  decodeJson :: Json -> Either JsonDecodeError Checklist
  decodeJson json = do
    dec <- decodeJson json
    content <- dec .: "content"
    name <- content .: "name"
    items <- content .: "items"
    either (const $ pure $ NewChecklist { content: { name: name, items: items } })
           (decodeServerChecklist name items)
           (dec .: "storageId")
    where
      decodeServerChecklist :: String -> Array ChecklistItem -> Object Json -> Either JsonDecodeError Checklist
      decodeServerChecklist name items storageIdObj = do
        version <- storageIdObj .: "version"
        id <- storageIdObj .: "id"
        pure $ ServerChecklist { content: { name: name, items: items }
                          , storageId: { version: version, id: id }}

instance checklistEncodeJson :: EncodeJson Checklist where
  encodeJson :: Checklist -> Json
  encodeJson (NewChecklist { content: checklistContent }) =
    cont ~> jsonEmptyObject
    where
      cont :: Tuple String Json
      cont = "content" := encodeContentObj checklistContent
  encodeJson (ServerChecklist { content: checklistContent, storageId: { version, id }}) =
    cont ~> storage ~> jsonEmptyObject
    where
      cont :: Tuple String Json
      cont = "content" := encodeContentObj checklistContent
      storage :: Tuple String Json
      storage = "storageId" := encodeStorageIdObj id version

instance checklistItemEncodeJson :: EncodeJson ChecklistItem where
  encodeJson :: ChecklistItem -> Json
  encodeJson (ChecklistItem { label, checked }) = 
    "label" := label
      ~> "checked" := checked
      ~> jsonEmptyObject

instance checklistItemDecodeJson :: DecodeJson ChecklistItem where
  decodeJson :: Json -> Either JsonDecodeError ChecklistItem 
  decodeJson json = do
    dec <- decodeJson json
    label <- dec .: "label"
    checked <- dec .: "checked"
    pure $ ChecklistItem { label: label, checked: checked }

encodeContentObj :: ChecklistContent -> Json
encodeContentObj { name, items } =
  "name" := name
    ~> "items" := items
    ~> jsonEmptyObject

encodeStorageIdObj :: String -> String -> Json
encodeStorageIdObj id version =
  "id" := id
    ~> "version" := version
    ~> jsonEmptyObject

type ChecklistId = Maybe { version :: String, id :: String }
             -- ^ The Nothing value represents the id of the NewChecklist
type ChecklistContent = { name  :: String
                        , items :: Array ChecklistItem
                        }

newtype ChecklistItem = ChecklistItem { label   :: String
                                      , checked :: Boolean
                                      }


type StorageId = { version :: String, id :: String }

data Action = Initialize
            | CreateNewChecklist
            | EditChecklistName Int
            | ChecklistNameChanged Int String
            | ChecklistLabelChanged Int Int String
            | EditDone
            | EditLabelContent Int Int
            | DeleteChecklist StorageId
            | DeleteChecklistItem Int Int

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
initialState = const { checklists: [], editingState: None }

newChecklist :: Checklist
newChecklist = NewChecklist { content: { name: "What's your new name?", items: [ ChecklistItem { label: "What's your new label?"
                                                                                               , checked: false } ] } }

-- ==================================== RENDERING ===========================================

render :: forall m. State -> H.ComponentHTML Action () m
render { checklists, editingState } =
  div []
    [ if (null checklists) then noChecklistsDiv else ul [ class_ "list-group" ] (mapWithIndex (checklistRender editingState) checklists)
    , section [ class_ "row" ]
        [ button [ class_ "btn btn-primary", onClick (const CreateNewChecklist) ] [ text "+" ] ]
    ]

noChecklistsDiv :: forall w i. HTML w i
noChecklistsDiv = div [ class_ "row" ] [ text "There are no checklists to display" ]

checklistRender :: forall w. EditingState -> Int -> Checklist -> HTML w Action
checklistRender editingState idx checklist =
  li [ class_ "row list-group-item" ] $
     [ div [ class_ "col", ref (wrap $ "checklist-" <> show idx) ] $
       [ checklistNameRender editingState idx (checklist ^. _name)
       , checklistContentRender editingState idx (checklist ^. _content) ]
       <> checklistFooterRender checklist
     ]

checklistFooterRender :: forall w. Checklist -> Array (HTML w Action)
checklistFooterRender (NewChecklist _) = []
checklistFooterRender (ServerChecklist { storageId }) =
  [ section [ class_ "row my-2 justify-content-center" ]
    [ button [ class_ "btn btn-sm btn-outline-danger", onClick (const $ DeleteChecklist storageId) ]
      [ i [ class_ "bi bi-trash" ] [] ]  
    ]
  ]

checklistContentRender :: forall w. EditingState -> Int -> ChecklistContent -> HTML w Action
checklistContentRender (EditingChecklistContent checklistIdx itemIdx) idx checklist
  | checklistIdx == idx = ul [ class_ "list-group" ] $ mapWithIndex (editChecklistItemRender itemIdx checklistIdx) checklist.items
checklistContentRender _ idx checklist = ul [ class_ "list-group" ] $ mapWithIndex (simpleChecklistItemRender idx) checklist.items

simpleChecklistItemRender :: forall w. Int -> Int -> ChecklistItem -> HTML w Action
simpleChecklistItemRender checklistIdx itemIdx (ChecklistItem { label }) =
  li [ class_ "list-group-item border-0", onClick (const $ EditLabelContent checklistIdx itemIdx) ]
    [  span [] [ text label ]
    , button [ type_ ButtonButton, class_ "btn btn-sm btn-danger", onClick (const $ DeleteChecklistItem checklistIdx itemIdx) ] [ i [ class_ "bi bi-trash" ] [] ]
    ]

editChecklistItemRender :: forall w. Int -> Int -> Int -> ChecklistItem -> HTML w Action 
editChecklistItemRender _ currentIdx currentChecklistIdx (ChecklistItem { label }) =
  li [ class_ "list-group-item border-0" ] [ input [ class_ "form-control label-input", onBlur (const EditDone), onValueChange  $ ChecklistLabelChanged currentChecklistIdx currentIdx, value label] ]

checklistNameRender :: forall w. EditingState -> Int -> String -> HTML w Action
checklistNameRender editingState idx name =
  header [ class_ "row my-2" ] [ contentRender editingState ]
  where
    contentRender (EditingChecklistName editIdx)
      | editIdx == idx = input [ class_ "form-control fs-2 lh-2 name-input", onBlur (const EditDone), onValueChange  $ ChecklistNameChanged idx, value name]
    contentRender _ = h2  [ onClick (const $ EditChecklistName idx) ] [ text name ]

-- ============================= Action Handling =======================================

handleAction :: Action -> ChecklistAppM Unit
handleAction action = handleError $
  case action of
    Initialize -> refreshChecklists
    CreateNewChecklist -> do
      st <- get
      H.put st { checklists = snoc st.checklists newChecklist
               , editingState = EditingChecklistName (length st.checklists)}
      goInput (length st.checklists)
    EditChecklistName idx -> do
      modify_ \st -> st { editingState = EditingChecklistName idx }
      goInput idx
    EditLabelContent checklistIdx itemIdx -> do
      modify_ \st -> st { editingState = EditingChecklistContent checklistIdx itemIdx }
      goInput checklistIdx
    ChecklistNameChanged idx newTitle -> updateChecklistWithSaveAndRefreshChecklists idx _name newTitle
    ChecklistLabelChanged checklistIdx itemIdx newLabel -> updateChecklistWithSaveAndRefreshChecklists checklistIdx (_checklistItems <<< ix itemIdx <<< _label) newLabel
    EditDone -> modify_ \st -> st { editingState = None }
    DeleteChecklist storageId -> do
      deleteChecklist storageId
      refreshChecklists
    DeleteChecklistItem checklistIdx itemIdx -> do
      checklists <- gets _.checklists
      let retrievedChecklist = checklists ^? ix checklistIdx
      maybe (throwError $ CustomFatalError ("Unable to retrieve checklist at index " <> show checklistIdx <> " while trying to delete one of its items"))
            (deleteChecklistItem itemIdx)
            retrievedChecklist


handleError :: ErrorChecklistAppM Unit -> ChecklistAppM Unit
handleError m = do
  res <- runExceptT m
  either logShow pure res

deleteChecklistItem :: Int -> Checklist -> ErrorChecklistAppM Unit
deleteChecklistItem itemIdx checklist =
  maybe (throwError $ CustomFatalError ("Unable to remove checklist item at index " <> show itemIdx))
        saveChecklistAndRefresh
        (removeChecklistItem itemIdx checklist)

removeChecklistItem :: Int -> Checklist -> Maybe Checklist
removeChecklistItem itemIdx (NewChecklist { content: { name, items } }) = do
  newItems <- deleteAt itemIdx items
  pure $ NewChecklist { content: { name, items: newItems } }
removeChecklistItem itemIdx (ServerChecklist { content: { name, items }, storageId }) = do
  newItems <- deleteAt itemIdx items
  pure $ ServerChecklist { content: { name, items: newItems }, storageId }

saveChecklistAndRefresh :: Checklist -> ErrorChecklistAppM Unit
saveChecklistAndRefresh checklist = do
  resp <- writeToServer checklist
  if unwrap resp.status >= 300 || unwrap resp.status < 200
    then throwError $ CustomFatalError $ "Wrong status response for post checklist: " <> show resp.status
    else refreshChecklists

updateChecklistWithSaveAndRefreshChecklists :: forall a. Int -> Traversal' Checklist a -> a -> ErrorChecklistAppM Unit
updateChecklistWithSaveAndRefreshChecklists idx lens_ newVal = do
  oldChecklists <- gets _.checklists
  let
    modifiedChecklist = index (snoc oldChecklists newChecklist) idx >>= ((lens_ .~ newVal) >>> pure)
  maybe (throwError $ CustomFatalError "Unable to modify checklist at index ")
        writeAndRefreshThenStopEditing
        modifiedChecklist
  where
    writeAndRefreshThenStopEditing :: Checklist -> ErrorChecklistAppM Unit
    writeAndRefreshThenStopEditing checklist = do
      saveChecklistAndRefresh checklist
      lift $ modify_ \st -> st { editingState = None }

goInput :: Int -> ErrorChecklistAppM Unit
goInput idx = do
  checklistElem <- getRef $ "checklist-" <> show idx
  scrollTo checklistElem
  nameElem <- focusName checklistElem
  maybe (throwError $ CustomFatalError $ "unable to get input element from name element") (liftEffect <<< select) $ InputElement.fromElement nameElem

scrollTo :: Element -> ErrorChecklistAppM Unit
scrollTo e = do
  w <- liftEffect window
  rect <- liftEffect $ getBoundingClientRect e
  liftEffect $ log $ "client rect: " <> show rect.y
  let elemMiddle = floor $ rect.y + rect.height / toNumber 2
  vh <- liftEffect $ innerHeight w
  liftEffect $ log $ "scrolling to " <> show (elemMiddle - vh / 2)
  liftEffect $ scroll 0 (max 0 (elemMiddle - vh / 2)) w

refreshChecklists :: ErrorChecklistAppM Unit
refreshChecklists = do
  newChecklists <- getChecklists
  lift $ modify_ \st -> st { checklists = newChecklists }

getChecklists :: ErrorChecklistAppM (Array Checklist)
getChecklists = do
  jsonResponse <- withExceptT toFatalError $ ExceptT $ liftAff $ Affjax.get json "/api/checklist"
  (_.body >>> decodeJson >>> lmap toFatalError >>> pure >>> ExceptT) jsonResponse

deleteChecklist :: StorageId -> ErrorChecklistAppM Unit
deleteChecklist { id } = do
  jsonResponse <- withExceptT toFatalError $ ExceptT $ liftAff $ delete json ("/api/checklist/" <> id)
  if unwrap jsonResponse.status < 200 || unwrap jsonResponse.status >= 300
    then (throwError $ CustomFatalError $ "Wrong status code when deleting checklist " <> id <> ": " <> show jsonResponse.status)
    else pure unit

isCreate :: Checklist -> Boolean
isCreate (NewChecklist _) = true
isCreate (ServerChecklist _) = false

writeToServer :: Checklist -> ErrorChecklistAppM (Response Json)
writeToServer checklist = withExceptT toFatalError $ ExceptT $ liftAff $ writeFunc json "/api/checklist" ((pure <<< Json <<< encodeJson) checklist)
  where
    writeFunc = if isCreate checklist then post else put

-- ====================== ERRORS ==============================================
data FatalError = DecodeError JsonDecodeError
                | NetworkError Error
                | CustomFatalError String

instance fatalErrorShowInstance :: Show FatalError where
  show (DecodeError err) = "DecodeError: " <> show err
  show (NetworkError err) = "NetworkError: " <> printError err
  show (CustomFatalError err) = "CustomError: " <> err

instance fatalErrorSemigroupInstance :: Semigroup FatalError where
  append _ last = last

instance jsonDecodeErrorToFatalErrorInstance :: ToFatalError JsonDecodeError where
  toFatalError = DecodeError

instance affjaxErrorToFatalErrorInstance :: ToFatalError Error where
  toFatalError = NetworkError

class ToFatalError a where
  toFatalError :: a -> FatalError

-- ============================  Web manipulation wrapper ==================================
getRef :: String -> ErrorChecklistAppM Element
getRef refStr = do
  ref <- lift $ H.getRef (wrap refStr) 
  maybe (throwError $ CustomFatalError $ "cannot get ref " <> refStr) pure ref

focusName :: Element -> ErrorChecklistAppM Element
focusName checklistElem = do
  name <- (getElementByClassName "name-input" checklistElem) <|> (getElementByClassName "content-input" checklistElem)
  focusElement name
  pure name

getElementByClassName :: String -> Element -> ErrorChecklistAppM Element
getElementByClassName className element = do
  name <- liftEffect $ getElementsByClassName className element >>= item 0
  maybe (throwError $ CustomFatalError $ "unable to get checklist name") pure name

focusElement :: Element -> ErrorChecklistAppM Unit
focusElement elem =
  maybe (throwError $ CustomFatalError $ "cannot convert element to HTML element") liftEffect $ (HTMLElement.fromElement elem >>= (pure <<< HTMLElement.focus))
