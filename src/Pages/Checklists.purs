module Pages.Checklists (component, removeChecklistItem, module Domain.Checklists) where

import Prelude hiding (div)

import Affjax.Web (Response)
import Api.Checklists (deleteChecklistResponse, getChecklistsResponse, writeChecklistResponse)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT(..), withExceptT)
import Control.Monad.RWS (get, gets, modify_)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Array (deleteAt, length, mapWithIndex, null, snoc)
import Data.Bifunctor (lmap)
import Data.Lens (Lens', Traversal', lens, lens', (.~), (^.), (^?))
import Data.Lens.Index (ix)
import Data.Maybe (Maybe, maybe)
import Data.Newtype (unwrap, wrap)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Domain.Checklists (Checklist(..), ChecklistContent, ChecklistItem(..), StorageId, newChecklist)
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, getRef, mkComponent, mkEval, put) as H
import Halogen.HTML (HTML, button, div, h2, header, i, input, li, section, span, text, ul)
import Halogen.HTML.Events (onBlur, onClick, onValueChange)
import Halogen.HTML.Properties (ButtonType(..), ref, type_, value)
import Ui.ErrorMessages (cannotConvertElement, cannotGetRef, unableToGetElement, unableToGetInputElement, wrongStatusDelete)
import Ui.Errors (FatalError(..), handleError, toFatalError)
import Ui.Focus (findElementByClassNames, focusElement, scrollToCenter, selectInputElement)
import Ui.PageFlow (saveAndRefresh, updateAtWithDefault)
import Ui.Utils (class_)
import Web.DOM (Element)

type NoOutput = Void
type ChecklistAppM = H.HalogenM State Action () NoOutput Aff
type ErrorChecklistAppM = ExceptT FatalError ChecklistAppM
type State =
  { checklists :: Array Checklist
  , editingState :: EditingState
  }

data EditingState
  = None
  | EditingChecklistName Int
  | EditingChecklistContent Int Int

_content :: Lens' Checklist ChecklistContent
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

data Action
  = Initialize
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
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = pure Initialize
        }
    }

initialState :: forall i. i -> State
initialState = const { checklists: [], editingState: None }

-- ==================================== RENDERING ===========================================

render :: forall m. State -> H.ComponentHTML Action () m
render { checklists, editingState } =
  div [ class_ "entity-page checklists-page" ]
    [ if (null checklists) then noChecklistsDiv else ul [ class_ "list-group entity-list checklists-list" ] (mapWithIndex (checklistRender editingState) checklists)
    , section [ class_ "row entity-add-row" ]
        [ button [ class_ "btn btn-primary entity-add-btn", onClick (const CreateNewChecklist) ] [ text "+" ] ]
    ]

noChecklistsDiv :: forall w i. HTML w i
noChecklistsDiv =
  div [ class_ "row entity-empty" ]
    [ div [ class_ "entity-empty-title" ] [ text "No checklist yet" ]
    , div [ class_ "entity-empty-subtitle" ] [ text "Create your first checklist and start tracking tasks." ]
    ]

checklistRender :: forall w. EditingState -> Int -> Checklist -> HTML w Action
checklistRender editingState idx checklist =
  li [ class_ "row list-group-item entity-card checklist-card" ] $
    [ div [ class_ "col entity-card-body", ref (wrap $ "checklist-" <> show idx) ] $
        [ checklistNameRender editingState idx (checklist ^. _name)
        , checklistContentRender editingState idx (checklist ^. _content)
        ]
          <> checklistFooterRender checklist
    ]

checklistFooterRender :: forall w. Checklist -> Array (HTML w Action)
checklistFooterRender (NewChecklist _) = []
checklistFooterRender (ServerChecklist { storageId }) =
  [ section [ class_ "row my-2 justify-content-center entity-footer" ]
      [ button [ class_ "btn btn-sm btn-outline-danger", onClick (const $ DeleteChecklist storageId) ]
          [ i [ class_ "bi bi-trash" ] [] ]
      ]
  ]

checklistContentRender :: forall w. EditingState -> Int -> ChecklistContent -> HTML w Action
checklistContentRender (EditingChecklistContent checklistIdx itemIdx) idx checklist
  | checklistIdx == idx = ul [ class_ "list-group checklist-items" ] $ mapWithIndex (editChecklistItemRender itemIdx checklistIdx) checklist.items
checklistContentRender _ idx checklist = ul [ class_ "list-group checklist-items" ] $ mapWithIndex (simpleChecklistItemRender idx) checklist.items

simpleChecklistItemRender :: forall w. Int -> Int -> ChecklistItem -> HTML w Action
simpleChecklistItemRender checklistIdx itemIdx (ChecklistItem { label }) =
  li [ class_ "list-group-item border-0 checklist-item-row", onClick (const $ EditLabelContent checklistIdx itemIdx) ]
    [ span [ class_ "checklist-item-label" ] [ text label ]
    , button [ type_ ButtonButton, class_ "btn btn-sm btn-danger", onClick (const $ DeleteChecklistItem checklistIdx itemIdx) ] [ i [ class_ "bi bi-trash" ] [] ]
    ]

editChecklistItemRender :: forall w. Int -> Int -> Int -> ChecklistItem -> HTML w Action
editChecklistItemRender _ currentIdx currentChecklistIdx (ChecklistItem { label }) =
  li [ class_ "list-group-item border-0 checklist-item-row" ] [ input [ class_ "form-control label-input checklist-item-input", onBlur (const EditDone), onValueChange $ ChecklistLabelChanged currentChecklistIdx currentIdx, value label ] ]

checklistNameRender :: forall w. EditingState -> Int -> String -> HTML w Action
checklistNameRender editingState idx name =
  header [ class_ "row my-2 checklist-title-row" ] [ contentRender editingState ]
  where
  contentRender (EditingChecklistName editIdx)
    | editIdx == idx = input [ class_ "form-control fs-2 lh-2 name-input checklist-title-input", onBlur (const EditDone), onValueChange $ ChecklistNameChanged idx, value name ]
  contentRender _ = h2 [ class_ "entity-title", onClick (const $ EditChecklistName idx) ] [ text name ]

-- ============================= Action Handling =======================================

handleAction :: Action -> ChecklistAppM Unit
handleAction action = handleError $
  case action of
    Initialize -> refreshChecklists
    CreateNewChecklist -> do
      st <- get
      H.put st
        { checklists = snoc st.checklists newChecklist
        , editingState = EditingChecklistName (length st.checklists)
        }
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
saveChecklistAndRefresh checklist =
  saveAndRefresh writeToServer refreshChecklists "checklist" checklist

updateChecklistWithSaveAndRefreshChecklists :: forall a. Int -> Traversal' Checklist a -> a -> ErrorChecklistAppM Unit
updateChecklistWithSaveAndRefreshChecklists idx lens_ newVal = do
  oldChecklists <- gets _.checklists
  let
    modifiedChecklist = updateAtWithDefault idx newChecklist (lens_ .~ newVal) oldChecklists
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
  liftEffect $ scrollToCenter checklistElem
  nameElem <- focusName checklistElem
  ok <- liftEffect $ selectInputElement nameElem
  if ok then pure unit
  else throwError $ CustomFatalError $ unableToGetInputElement "name"

refreshChecklists :: ErrorChecklistAppM Unit
refreshChecklists = do
  newChecklists <- getChecklists
  lift $ modify_ \st -> st { checklists = newChecklists }

getChecklists :: ErrorChecklistAppM (Array Checklist)
getChecklists = do
  jsonResponse <- withExceptT toFatalError $ ExceptT $ liftAff getChecklistsResponse
  (_.body >>> decodeJson >>> lmap toFatalError >>> pure >>> ExceptT) jsonResponse

deleteChecklist :: StorageId -> ErrorChecklistAppM Unit
deleteChecklist storageId@{ id } = do
  jsonResponse <- withExceptT toFatalError $ ExceptT $ liftAff $ deleteChecklistResponse storageId
  if unwrap jsonResponse.status < 200 || unwrap jsonResponse.status >= 300 then throwError $ CustomFatalError $ wrongStatusDelete "checklist" id jsonResponse.status
  else pure unit

writeToServer :: Checklist -> ErrorChecklistAppM (Response Json)
writeToServer checklist = withExceptT toFatalError $ ExceptT $ liftAff $ writeChecklistResponse checklist

-- ============================  Web manipulation wrapper ==================================
getRef :: String -> ErrorChecklistAppM Element
getRef refStr = do
  ref <- lift $ H.getRef (wrap refStr)
  maybe (throwError $ CustomFatalError $ cannotGetRef refStr) pure ref

focusName :: Element -> ErrorChecklistAppM Element
focusName checklistElem = do
  name <- liftEffect $ findElementByClassNames [ "name-input", "content-input" ] checklistElem
  elem <- maybe (throwError $ CustomFatalError $ unableToGetElement "checklist name") pure name
  ok <- liftEffect $ focusElement elem
  if ok then pure elem
  else throwError $ CustomFatalError cannotConvertElement
