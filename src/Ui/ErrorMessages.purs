module Ui.ErrorMessages
  ( cannotConvertElement
  , cannotGetRef
  , unableToGetElement
  , unableToGetInputElement
  , wrongStatusDelete
  , wrongStatusPost
  ) where

import Prelude

cannotGetRef :: String -> String
cannotGetRef refStr = "cannot get ref " <> refStr

cannotConvertElement :: String
cannotConvertElement = "cannot convert element to HTML element"

unableToGetInputElement :: String -> String
unableToGetInputElement label = "unable to get input element from " <> label <> " element"

unableToGetElement :: String -> String
unableToGetElement label = "unable to get " <> label

wrongStatusPost :: forall a. Show a => String -> a -> String
wrongStatusPost entity status =
  "Wrong status response for post " <> entity <> ": " <> show status

wrongStatusDelete :: forall a. Show a => String -> String -> a -> String
wrongStatusDelete entity entityId status =
  "Wrong status code when deleting " <> entity <> " " <> entityId <> ": " <> show status
