module Domain.Checklists
  ( Checklist(..)
  , ChecklistContent
  , ChecklistItem(..)
  , StorageId
  , newChecklist
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut.Core (Json, jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Argonaut.Decode.Error (JsonDecodeError)
import Data.Argonaut.Encode (class EncodeJson, (:=), (~>))
import Data.Either (Either, either)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple)
import Foreign.Object (Object)

data Checklist
  = NewChecklist { content :: ChecklistContent }
  | ServerChecklist
      { content :: ChecklistContent
      , storageId :: StorageId
      }

derive instance checklistGenericInstance :: Generic Checklist _
derive instance checklistEqInstance :: Eq Checklist
instance checklistShowInstance :: Show Checklist where
  show = genericShow

newtype ChecklistItem = ChecklistItem { label :: String, checked :: Boolean }

derive instance checklistItemNewtypeInstance :: Newtype ChecklistItem _
derive newtype instance checklistItemEqInstance :: Eq ChecklistItem
derive newtype instance checklistItemShowInstance :: Show ChecklistItem

type ChecklistContent =
  { name :: String
  , items :: Array ChecklistItem
  }

type StorageId = { version :: String, id :: String }

instance checklistDecodeJsonInstance :: DecodeJson Checklist where
  decodeJson :: Json -> Either JsonDecodeError Checklist
  decodeJson json = decodeWrappedChecklist json <|> decodeFlatChecklist json
    where
    decodeWrappedChecklist :: Json -> Either JsonDecodeError Checklist
    decodeWrappedChecklist wrappedJson = do
      dec <- decodeJson wrappedJson
      content <- dec .: "content"
      name <- content .: "name"
      items <- content .: "items"
      either (const $ pure $ NewChecklist { content: { name: name, items: items } })
        (decodeServerChecklist name items)
        (dec .: "storageId")

    decodeFlatChecklist :: Json -> Either JsonDecodeError Checklist
    decodeFlatChecklist flatJson = do
      dec <- decodeJson flatJson
      name <- dec .: "name"
      items <- dec .: "items"
      pure $ NewChecklist { content: { name: name, items: items } }

    decodeServerChecklist :: String -> Array ChecklistItem -> Object Json -> Either JsonDecodeError Checklist
    decodeServerChecklist name items storageIdObj = do
      version <- storageIdObj .: "version"
      id <- storageIdObj .: "id"
      pure $ ServerChecklist
        { content: { name: name, items: items }
        , storageId: { version: version, id: id }
        }

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

instance checklistEncodeJson :: EncodeJson Checklist where
  encodeJson :: Checklist -> Json
  encodeJson (NewChecklist { content: { name, items } }) =
    encodeContentObj name items
  encodeJson (ServerChecklist { content: { name, items }, storageId: { version, id } }) =
    cont ~> storage ~> jsonEmptyObject
    where
    cont :: Tuple String Json
    cont = "content" := encodeContentObj name items

    storage :: Tuple String Json
    storage = "storageId" := encodeStorageIdObj id version

encodeContentObj :: String -> Array ChecklistItem -> Json
encodeContentObj name items =
  "name" := name
    ~> "items" := items
    ~> jsonEmptyObject

encodeStorageIdObj :: String -> String -> Json
encodeStorageIdObj id version =
  "id" := id
    ~> "version" := version
    ~> jsonEmptyObject

newChecklist :: Checklist
newChecklist =
  NewChecklist
    { content:
        { name: "What's your new name?"
        , items:
            [ ChecklistItem { label: "What's your new label?", checked: false }
            ]
        }
    }
