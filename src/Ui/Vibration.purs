module Ui.Vibration (vibrateIfAvailable) where

import Prelude (Unit)
import Effect (Effect)

foreign import vibrateIfAvailable :: Int -> Effect Unit
