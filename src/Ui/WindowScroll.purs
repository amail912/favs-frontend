module Ui.WindowScroll
  ( getWindowScrollY
  , setWindowScrollY
  ) where

import Prelude

import Effect (Effect)

foreign import getWindowScrollY :: Effect Int
foreign import setWindowScrollY :: Int -> Effect Unit
