module Ui.Errors
  ( FatalError(..)
  , class ToFatalError
  , toFatalError
  , handleError
  ) where

import Prelude

import Affjax (Error, printError)
import Control.Monad.Except (ExceptT, runExceptT)
import Data.Argonaut.Decode.Error (JsonDecodeError)
import Data.Either (either)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (logShow)

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

handleError :: forall m e. MonadEffect m => Show e => ExceptT e m Unit -> m Unit
handleError m = do
  res <- runExceptT m
  either logShow pure res
