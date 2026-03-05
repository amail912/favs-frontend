module Ui.AgendaRender
  ( renderPanelHeader
  ) where

import Prelude hiding (div)

import Halogen.HTML (HTML, div, text)
import Ui.Utils (class_)

renderPanelHeader
  :: forall w i
   . { baseClass :: String, title :: String, subtitle :: String }
  -> Array (HTML w i)
  -> HTML w i
renderPanelHeader config actions =
  div [ class_ $ config.baseClass <> "-header" ]
    ( [ div []
          [ div [ class_ $ config.baseClass <> "-title" ] [ text config.title ]
          , div [ class_ $ config.baseClass <> "-subtitle" ] [ text config.subtitle ]
          ]
      ] <> actions
    )
