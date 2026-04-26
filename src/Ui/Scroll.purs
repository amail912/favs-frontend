module Ui.Scroll
  ( scrollElementIntoView
  ) where

import Prelude

import Effect (Effect)
import Web.DOM.Element (Element)

foreign import scrollElementIntoView :: Element -> Effect Unit
