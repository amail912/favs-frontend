module Domain.Notes
  ( Note(..)
  , NoteContent
  , NoteId
  , StorageId
  , newNote
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut.Core (Json, jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Argonaut.Decode.Error (JsonDecodeError)
import Data.Argonaut.Encode (class EncodeJson, (:=), (~>))
import Data.Either (Either, either)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple)
import Foreign.Object (Object)


data Note = NewNote { content :: NoteContent }
          | ServerNote { content :: NoteContent
                       , storageId :: StorageId }

derive instance noteGenericInstance :: Generic Note _
derive instance noteEqInstance :: Eq Note
instance noteShowInstance :: Show Note where
  show = genericShow

instance noteDecodeJsonInstance :: DecodeJson Note where
  decodeJson :: Json -> Either JsonDecodeError Note
  decodeJson json = decodeWrappedNote json <|> decodeFlatNewNote json
    where
      decodeWrappedNote :: Json -> Either JsonDecodeError Note
      decodeWrappedNote wrappedJson = do
        dec <- decodeJson wrappedJson
        content <- dec .: "content"
        title <- content .: "title"
        noteContent <- content .: "noteContent"
        either (const $ pure $ NewNote { content: { title: title, noteContent: noteContent } })
               (decodeServerNote title noteContent)
               (dec .: "storageId")

      decodeFlatNewNote :: Json -> Either JsonDecodeError Note
      decodeFlatNewNote flatJson = do
        dec <- decodeJson flatJson
        title <- dec .: "title"
        noteContent <- dec .: "noteContent"
        pure $ NewNote { content: { title: title, noteContent: noteContent } }

      decodeServerNote :: String -> String -> Object Json -> Either JsonDecodeError Note
      decodeServerNote title noteContent storageIdObj = do
        version <- storageIdObj .: "version"
        id <- storageIdObj .: "id"
        pure $ ServerNote { content: { title: title, noteContent: noteContent }
                          , storageId: { version: version, id: id }}

instance noteEncodeJson :: EncodeJson Note where
  encodeJson :: Note -> Json
  encodeJson (NewNote { content: { title, noteContent } }) =
    encodeContentObj title noteContent
  encodeJson (ServerNote { content: { title, noteContent }, storageId: { version, id }}) =
    cont ~> storage ~> jsonEmptyObject
    where
      cont :: Tuple String Json
      cont = "content" := encodeContentObj title noteContent
      storage :: Tuple String Json
      storage = "storageId" := encodeStorageIdObj id version

encodeContentObj :: String -> String -> Json
encodeContentObj title noteContent =
  "title" := title
    ~> "noteContent" := noteContent
    ~> jsonEmptyObject

encodeStorageIdObj :: String -> String -> Json
encodeStorageIdObj id version =
  "id" := id
    ~> "version" := version
    ~> jsonEmptyObject

type NoteId = Maybe { version :: String, id :: String }
-- ^ The Nothing value represents the id of the NewNote

type NoteContent = { noteContent :: String, title :: String }

type StorageId = { version :: String, id :: String }

newNote :: Note
newNote = NewNote { content: { title: "What's your new title?", noteContent: "What's your new content?" } }
