module Ui.CreateButton (renderIconCreateButton) where

import Prelude

import Halogen.HTML (HTML, button, i)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (attr)
import Halogen.HTML.Core (AttrName(..))
import Ui.Utils (class_)

renderIconCreateButton :: forall w i. String -> String -> i -> HTML w i
renderIconCreateButton className ariaLabel action =
  button
    [ class_ className
    , attr (AttrName "aria-label") ariaLabel
    , onClick (const action)
    ]
    [ i [ class_ "bi bi-plus" ] [] ]
