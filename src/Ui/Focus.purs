module Ui.Focus
  ( scrollToCenter
  , findElementByClassName
  , findElementByClassNames
  , focusElement
  , selectInputElement
  ) where

import Prelude

import Data.Array (uncons)
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Web.DOM (Element)
import Web.DOM.Element (getBoundingClientRect, getElementsByClassName)
import Web.DOM.HTMLCollection (item)
import Web.HTML (window)
import Web.HTML.HTMLElement (focus, fromElement) as HTMLElement
import Web.HTML.HTMLInputElement (fromElement) as InputElement
import Web.HTML.HTMLInputElement (select)
import Web.HTML.Window (innerHeight, scroll)

scrollToCenter :: Element -> Effect Unit
scrollToCenter e = do
  w <- window
  rect <- getBoundingClientRect e
  log $ "client rect: " <> show rect.y
  let elemMiddle = floor $ rect.y + rect.height / toNumber 2
  vh <- innerHeight w
  log $ "scrolling to " <> show (elemMiddle - vh / 2)
  scroll 0 (max 0 (elemMiddle - vh / 2)) w

findElementByClassName :: String -> Element -> Effect (Maybe Element)
findElementByClassName className element =
  getElementsByClassName className element >>= item 0

findElementByClassNames :: Array String -> Element -> Effect (Maybe Element)
findElementByClassNames classNames element =
  case uncons classNames of
    Nothing -> pure Nothing
    Just { head, tail } -> do
      found <- findElementByClassName head element
      case found of
        Just _ -> pure found
        Nothing -> findElementByClassNames tail element

focusElement :: Element -> Effect Boolean
focusElement elem =
  case HTMLElement.fromElement elem of
    Nothing -> pure false
    Just htmlElem -> HTMLElement.focus htmlElem *> pure true

selectInputElement :: Element -> Effect Boolean
selectInputElement elem =
  case InputElement.fromElement elem of
    Nothing -> pure false
    Just inputElem -> select inputElem *> pure true
