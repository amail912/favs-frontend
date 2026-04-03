module Ui.LocalStorage
  ( getItem
  , setItem
  , removeItem
  ) where

import Prelude

import Effect (Effect)

foreign import getItem :: String -> Effect String

foreign import setItem :: String -> String -> Effect Unit

foreign import removeItem :: String -> Effect Unit
