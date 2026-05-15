module Ui.CreateButton (renderFabButton, withFabButton) where

import Prelude

import Halogen.HTML (HTML, button, i)
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (attr)
import Halogen.HTML.Core (AttrName(..))
import Ui.Utils (class_)

renderFabButton :: forall w i. String -> String -> i -> HTML w i
renderFabButton className ariaLabel action =
  button
    [ class_ className
    , attr (AttrName "aria-label") ariaLabel
    , onClick (const action)
    ]
    [ i [ class_ "bi bi-plus" ] [] ]

withFabButton :: forall w i. { fabClass :: String, fabAriaLabel :: String, fabAction :: i } -> HTML w i -> HTML w i
withFabButton { fabClass, fabAriaLabel, fabAction } content =
  HH.div []
    [ content
    , renderFabButton fabClass fabAriaLabel fabAction
    ]
