module Agenda.Import
  ( ImportState
  , ImportAction(..)
  , ImportCtx
  , importInitialState
  , handleImportAction
  , renderCsvImportPanel
  , renderIcsImportPanel
  ) where

import Prelude hiding (div)

import Agenda.Commands (ImportCommand(..), Command(..), tellCmd)
import Agenda.Imports (parseCsvImport, parseIcsImport)
import Agenda.Model (CalendarItem, CsvImportError, CsvImportResult, IcsImportError, IcsImportResult)
import Agenda.Offline (applyOfflineMutation)
import Control.Monad.State.Trans (StateT, get, modify_)
import Control.Monad.Writer.Trans (WriterT)
import Data.Array (foldl, length, null)
import Data.Lens (Lens', (.~), (^.))
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff (Aff)
import Halogen.HTML (HTML, button, div, section, textarea, text)
import Halogen.HTML.Events (onClick, onValueChange)
import Halogen.HTML.Properties (placeholder, value)
import Type.Proxy (Proxy(..))
import Ui.AgendaRender (renderPanelHeader)
import Ui.Utils (class_)


type ImportState =
  { csvInput :: String
  , csvImportResult :: Maybe CsvImportResult
  , icsInput :: String
  , icsImportResult :: Maybe IcsImportResult
  }


type ImportCtx =
  { items :: Array CalendarItem
  , pending :: Array CalendarItem
  , offlineMode :: Boolean
  }


importInitialState :: ImportState
importInitialState =
  { csvInput: ""
  , csvImportResult: Nothing
  , icsInput: ""
  , icsImportResult: Nothing
  }


_csvInputS :: Lens' ImportState String
_csvInputS = prop (Proxy :: _ "csvInput")

_csvImportResultS :: Lens' ImportState (Maybe CsvImportResult)
_csvImportResultS = prop (Proxy :: _ "csvImportResult")

_icsInputS :: Lens' ImportState String
_icsInputS = prop (Proxy :: _ "icsInput")

_icsImportResultS :: Lens' ImportState (Maybe IcsImportResult)
_icsImportResultS = prop (Proxy :: _ "icsImportResult")


data ImportAction
  = ImportCsvInputChanged String
  | ImportParseCsvInput
  | ImportApplyCsv
  | ImportClearCsv
  | ImportIcsInputChanged String
  | ImportParseIcsInput
  | ImportApplyIcs
  | ImportClearIcs


handleImportAction :: ImportCtx -> ImportAction -> StateT ImportState (WriterT (Array Command) Aff) Unit
handleImportAction ctx = case _ of
  ImportCsvInputChanged raw ->
    modify_ (_csvInputS .~ raw)
  ImportParseCsvInput -> do
    st <- get
    let result = parseCsvImport (st ^. _csvInputS)
    modify_ (_csvImportResultS .~ Just result)
  ImportApplyCsv -> do
    importState <- get
    case importState ^. _csvImportResultS of
      Nothing -> pure unit
      Just result ->
        if null result.items then pure unit
        else do
          if ctx.offlineMode then do
            let
              initial = { items: ctx.items, pending: ctx.pending }
              final = foldl (\acc item -> applyOfflineMutation true item acc.items acc.pending) initial result.items
            tellCmd $ ImportCmd (ImportSetItems final.items)
            tellCmd $ ImportCmd (ImportSetPending final.pending)
          else tellCmd $ ImportCmd (ImportSetItems (ctx.items <> result.items))
          modify_ ((_csvInputS .~ "") <<< (_csvImportResultS .~ Nothing))
  ImportClearCsv ->
    modify_ ((_csvInputS .~ "") <<< (_csvImportResultS .~ Nothing))
  ImportIcsInputChanged raw ->
    modify_ (_icsInputS .~ raw)
  ImportParseIcsInput -> do
    st <- get
    let result = parseIcsImport (st ^. _icsInputS)
    modify_ (_icsImportResultS .~ Just result)
  ImportApplyIcs -> do
    importState <- get
    case importState ^. _icsImportResultS of
      Nothing -> pure unit
      Just result ->
        if null result.items then pure unit
        else do
          if ctx.offlineMode then do
            let
              initial = { items: ctx.items, pending: ctx.pending }
              final = foldl (\acc item -> applyOfflineMutation true item acc.items acc.pending) initial result.items
            tellCmd $ ImportCmd (ImportSetItems final.items)
            tellCmd $ ImportCmd (ImportSetPending final.pending)
          else tellCmd $ ImportCmd (ImportSetItems (ctx.items <> result.items))
          modify_ ((_icsInputS .~ "") <<< (_icsImportResultS .~ Nothing))
  ImportClearIcs ->
    modify_ ((_icsInputS .~ "") <<< (_icsImportResultS .~ Nothing))


renderCsvImportPanel :: forall w. String -> Maybe CsvImportResult -> HTML w ImportAction
renderCsvImportPanel csvInput result =
  section [ class_ "agenda-import" ]
    [ renderPanelHeader
        "agenda-import"
        "Import CSV"
        "Colonnes minimales: type, titre, fenetre_debut, fenetre_fin."
        []
    , textarea
        [ class_ "form-control agenda-import-textarea"
        , placeholder "Collez votre CSV ici..."
        , value csvInput
        , onValueChange ImportCsvInputChanged
        ]
    , div [ class_ "agenda-import-actions" ]
        [ button [ class_ "btn btn-sm btn-outline-primary", onClick (const ImportParseCsvInput) ] [ text "Analyser" ]
        , button [ class_ "btn btn-sm btn-outline-secondary", onClick (const ImportClearCsv) ] [ text "Effacer" ]
        , button [ class_ "btn btn-sm btn-success", onClick (const ImportApplyCsv) ] [ text "Ajouter a la liste" ]
        ]
    , maybe (text "") renderCsvImportResult result
    ]

renderCsvImportResult :: forall w action. CsvImportResult -> HTML w action
renderCsvImportResult result =
  let
    okCount = length result.items
    errorCount = length result.errors
  in
    div [ class_ "agenda-import-result" ]
      [ div [ class_ "agenda-import-summary" ]
          [ text $ "Valides: " <> show okCount <> " • Erreurs: " <> show errorCount ]
      , if null result.errors then text "" else renderCsvImportErrors result.errors
      ]

renderCsvImportErrors :: forall w action. Array CsvImportError -> HTML w action
renderCsvImportErrors errors =
  div [ class_ "agenda-import-errors" ]
    (map renderCsvImportError errors)

renderCsvImportError :: forall w action. CsvImportError -> HTML w action
renderCsvImportError err =
  div [ class_ "agenda-import-error" ]
    [ text $ "Ligne " <> show err.rowNumber <> ": " <> err.message ]

renderIcsImportPanel :: forall w. String -> Maybe IcsImportResult -> HTML w ImportAction
renderIcsImportPanel icsInput result =
  section [ class_ "agenda-import" ]
    [ renderPanelHeader
        "agenda-import"
        "Import ICS"
        "Support basique: SUMMARY, DTSTART, DTEND."
        []
    , textarea
        [ class_ "form-control agenda-import-textarea"
        , placeholder "Collez votre fichier ICS ici..."
        , value icsInput
        , onValueChange ImportIcsInputChanged
        ]
    , div [ class_ "agenda-import-actions" ]
        [ button [ class_ "btn btn-sm btn-outline-primary", onClick (const ImportParseIcsInput) ] [ text "Analyser" ]
        , button [ class_ "btn btn-sm btn-outline-secondary", onClick (const ImportClearIcs) ] [ text "Effacer" ]
        , button [ class_ "btn btn-sm btn-success", onClick (const ImportApplyIcs) ] [ text "Ajouter a la liste" ]
        ]
    , maybe (text "") renderIcsImportResult result
    ]

renderIcsImportResult :: forall w action. IcsImportResult -> HTML w action
renderIcsImportResult result =
  let
    okCount = length result.items
    errorCount = length result.errors
  in
    div [ class_ "agenda-import-result" ]
      [ div [ class_ "agenda-import-summary" ]
          [ text $ "Valides: " <> show okCount <> " • Erreurs: " <> show errorCount ]
      , if null result.errors then text "" else renderIcsImportErrors result.errors
      ]

renderIcsImportErrors :: forall w action. Array IcsImportError -> HTML w action
renderIcsImportErrors errors =
  div [ class_ "agenda-import-errors" ]
    (map renderIcsImportError errors)

renderIcsImportError :: forall w action. IcsImportError -> HTML w action
renderIcsImportError err =
  div [ class_ "agenda-import-error" ]
    [ text $ "Evenement " <> show err.eventIndex <> ": " <> err.message ]
