module Ui.AgendaRender
  ( renderPanelHeader
  ) where

import Prelude hiding (div)

import Halogen.HTML (HTML, div, text)
import Ui.Utils (class_)

renderPanelHeader :: forall w i. String -> String -> String -> Array (HTML w i) -> HTML w i
renderPanelHeader baseClass title subtitle actions =
  div [ class_ $ baseClass <> "-header" ]
    ([ div []
         [ div [ class_ $ baseClass <> "-title" ] [ text title ]
         , div [ class_ $ baseClass <> "-subtitle" ] [ text subtitle ]
         ]
     ] <> actions)
