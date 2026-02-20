module Pages.Notes (component, module Domain.Notes) where

import Prelude hiding (div)

import Affjax.Web (Response)
import Api.Notes (deleteNoteResponse, getNotesResponse, writeNoteResponse)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT(..), withExceptT)
import Control.Monad.RWS (get, gets, modify_)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Array (length, mapWithIndex, null, snoc)
import Data.Bifunctor (lmap)
import Data.Lens (Lens', lens, lens', (.~), (^.))
import Data.Maybe (maybe)
import Data.Newtype (unwrap, wrap)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Domain.Notes (Note(..), NoteContent, StorageId, newNote)
import Halogen (Component, ComponentHTML, HalogenM, defaultEval, getRef, mkComponent, mkEval, put) as H
import Halogen.HTML (HTML, button, div, h2, header, i, input, li, section, text, textarea, ul)
import Halogen.HTML.Events (onBlur, onClick, onValueChange)
import Halogen.HTML.Properties (ref, value)
import Ui.ErrorMessages (cannotConvertElement, cannotGetRef, unableToGetElement, unableToGetInputElement, wrongStatusDelete)
import Ui.Errors (FatalError(..), handleError, toFatalError)
import Ui.Focus (findElementByClassNames, focusElement, scrollToCenter, selectInputElement)
import Ui.PageFlow (saveAndRefresh, updateAtWithDefault)
import Ui.Utils (class_)
import Web.DOM (Element)

type NoOutput = Void
type NoteAppM = H.HalogenM State Action () NoOutput Aff
type ErrorNoteAppM = ExceptT FatalError NoteAppM
type State = { notes :: Array Note
             , editingState :: EditingState
             }

data EditingState = None
                  | EditingNoteTitle Int
                  | EditingNoteContent Int

_content :: Lens' Note NoteContent
_content = lens' $ (\note -> Tuple (getContent note) (setContent note))


getContent :: Note -> NoteContent
getContent (NewNote n) = n.content
getContent (ServerNote n) = n.content

setContent :: Note -> NoteContent -> Note
setContent (NewNote _) newContent = NewNote { content: newContent }
setContent (ServerNote n) newContent = ServerNote (n { content = newContent })

_title :: Lens' Note String
_title = _content <<< (lens _.title $ _ { title = _ })

_noteContent :: Lens' Note String
_noteContent = _content <<< (lens _.noteContent $ _ { noteContent = _ })

data Action = Initialize
            | CreateNewNote
            | EditNoteTitle Int
            | NoteTitleChanged Int String
            | NoteContentChanged Int String
            | EditDone
            | EditNoteContent Int
            | DeleteNote StorageId

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
initialState = const { notes: [], editingState: None }

-- ==================================== RENDERING ===========================================

render :: forall m. State -> H.ComponentHTML Action () m
render { notes, editingState } =
  div [ class_ "entity-page notes-page" ]
    [ if (null notes) then noNotesDiv else ul [ class_ "list-group entity-list notes-list" ] (mapWithIndex (noteRender editingState) notes)
    , section [ class_ "row entity-add-row" ]
        [ button [ class_ "btn btn-primary entity-add-btn", onClick (const CreateNewNote) ] [ text "+" ] ]
    ]

noNotesDiv :: forall w i. HTML w i
noNotesDiv =
  div [ class_ "row entity-empty" ]
    [ div [ class_ "entity-empty-title" ] [ text "No notes yet" ]
    , div [ class_ "entity-empty-subtitle" ] [ text "Create your first note to get started." ]
    ]

noteRender :: forall w. EditingState -> Int -> Note -> HTML w Action
noteRender editingState idx note =
  li [ class_ "row list-group-item entity-card note-card" ] $
     [ div [ class_ "col entity-card-body", ref (wrap $ "note-" <> show idx) ] $
       [ noteTitleRender editingState idx note
       , noteContentRender editingState idx note ]
       <> noteFooterRender note
     ]

noteFooterRender :: forall w. Note -> Array (HTML w Action)
noteFooterRender (NewNote _) = []
noteFooterRender (ServerNote { storageId }) =
  [ section [ class_ "row my-2 justify-content-center entity-footer" ]
    [ button [ class_ "btn btn-sm btn-outline-danger", onClick (const $ DeleteNote storageId) ]
      [ i [ class_ "bi bi-trash" ] [] ]  
    ]
  ]

noteContentRender :: forall w. EditingState -> Int -> Note -> HTML w Action
noteContentRender editingState idx note =
  section [ class_ "row my-2 note-content-row" ] [ contentRender editingState ]
  where
    contentRender (EditingNoteContent editIdx)
      | editIdx == idx = textarea [ class_ "form-control content-input note-content-input", onBlur (const EditDone), onValueChange  $ NoteContentChanged idx, value (note ^. _noteContent) ]
    contentRender _ = div [ class_ "note-content-preview", onClick (const $ EditNoteContent idx) ] [ text (note ^. _noteContent) ]

noteTitleRender :: forall w. EditingState -> Int -> Note -> HTML w Action
noteTitleRender editingState idx note =
  header [ class_ "row my-2 note-title-row" ] [ contentRender editingState ]
  where
    contentRender (EditingNoteTitle editIdx)
      | editIdx == idx = input [ class_ "form-control fs-2 lh-2 title-input note-title-input", onBlur (const EditDone), onValueChange  $ NoteTitleChanged idx, value (note ^. _title)]
    contentRender _ = h2 [ class_ "entity-title", onClick (const $ EditNoteTitle idx) ] [ text (note ^. _title) ]

-- ============================= Action Handling =======================================

handleAction :: Action -> NoteAppM Unit
handleAction action = handleError $
  case action of
    Initialize -> refreshNotes
    CreateNewNote -> do
      st <- get
      H.put st { notes = snoc st.notes newNote
               , editingState = EditingNoteTitle (length st.notes)}
      goInput (length st.notes)
    EditNoteTitle idx -> do
      modify_ \st -> st { editingState = EditingNoteTitle idx }
      goInput idx
    EditNoteContent idx -> do
      modify_ \st -> st { editingState = EditingNoteContent idx }
      goInput idx
    NoteTitleChanged idx newTitle -> updateNoteWithSaveAndRefreshNotes idx _title newTitle
    NoteContentChanged idx newContent -> updateNoteWithSaveAndRefreshNotes idx _noteContent newContent
    EditDone -> modify_ \st -> st { editingState = None }
    DeleteNote storageId -> do
      deleteNote storageId
      refreshNotes

updateNoteWithSaveAndRefreshNotes :: forall a. Int -> Lens' Note a -> a -> ErrorNoteAppM Unit
updateNoteWithSaveAndRefreshNotes idx lens_ newVal = do
  oldNotes <- gets _.notes
  let
    modifiedNote = updateAtWithDefault idx newNote (lens_ .~ newVal) oldNotes
  maybe (throwError $ CustomFatalError "Unable to modify note at index ")
        writeAndRefreshThenStopEditing
        modifiedNote
  where
    writeAndRefreshThenStopEditing :: Note -> ErrorNoteAppM Unit
    writeAndRefreshThenStopEditing note = do
      saveAndRefresh writeToServer refreshNotes "note" note
      lift $ modify_ \st -> st { editingState = None }

goInput :: Int -> ErrorNoteAppM Unit
goInput idx = do
  noteElem <- getRef $ "note-" <> show idx
  liftEffect $ scrollToCenter noteElem
  titleElem <- focusTitle noteElem
  ok <- liftEffect $ selectInputElement titleElem
  if ok then pure unit
  else throwError $ CustomFatalError $ unableToGetInputElement "title"

refreshNotes :: ErrorNoteAppM Unit
refreshNotes = do
  newNotes <- getNotes
  lift $ modify_ \st -> st { notes = newNotes }

getNotes :: ErrorNoteAppM (Array Note)
getNotes = do
  jsonResponse <- withExceptT toFatalError $ ExceptT $ liftAff getNotesResponse
  (_.body >>> decodeJson >>> lmap toFatalError >>> pure >>> ExceptT) jsonResponse

deleteNote :: StorageId -> ErrorNoteAppM Unit
deleteNote storageId@{ id } = do
  jsonResponse <- withExceptT toFatalError $ ExceptT $ liftAff $ deleteNoteResponse storageId
  if unwrap jsonResponse.status < 200 || unwrap jsonResponse.status >= 300
    then throwError $ CustomFatalError $ wrongStatusDelete "note" id jsonResponse.status
    else pure unit

writeToServer :: Note -> ErrorNoteAppM (Response Json)
writeToServer note = withExceptT toFatalError $ ExceptT $ liftAff $ writeNoteResponse note

-- ============================  Web manipulation wrapper ==================================
getRef :: String -> ErrorNoteAppM Element
getRef refStr = do
  ref <- lift $ H.getRef (wrap refStr) 
  maybe (throwError $ CustomFatalError $ cannotGetRef refStr) pure ref

focusTitle :: Element -> ErrorNoteAppM Element
focusTitle noteElem = do
  title <- liftEffect $ findElementByClassNames [ "title-input", "content-input" ] noteElem
  elem <- maybe (throwError $ CustomFatalError $ unableToGetElement "note title") pure title
  ok <- liftEffect $ focusElement elem
  if ok then pure elem
  else throwError $ CustomFatalError cannotConvertElement
