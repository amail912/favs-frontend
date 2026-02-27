module Ui.PageFlow
  ( updateAtWithDefault
  , saveAndRefresh
  ) where

import Prelude

import Affjax (Response)
import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Argonaut.Core (Json)
import Data.Array (index, snoc)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Ui.ErrorMessages (wrongStatusPost)
import Ui.Errors (FatalError(..))

updateAtWithDefault :: forall a. Int -> a -> (a -> a) -> Array a -> Maybe a
updateAtWithDefault idx defaultItem update items =
  index (snoc items defaultItem) idx <#> update

saveAndRefresh
  :: forall m a
   . MonadError FatalError m
  => (a -> m (Response Json))
  -> m Unit
  -> String
  -> a
  -> m Unit
saveAndRefresh save refresh entity item = do
  resp <- save item
  if unwrap resp.status >= 200 && unwrap resp.status < 300 then refresh
  else throwError $ CustomFatalError $ wrongStatusPost entity resp.status
