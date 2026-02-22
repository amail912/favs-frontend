module Ui.Modal
  ( renderModal
  ) where

import Prelude hiding (div)

import Halogen.HTML (HTML, button, div, text)
import Halogen.HTML.Events (onClick)
import Ui.Utils (class_)

renderModal :: forall w i. String -> Boolean -> i -> Array (HTML w i) -> HTML w i
renderModal title isOpen closeAction content =
  if not isOpen then text ""
  else
    div [ class_ "app-modal" ]
      [ div [ class_ "app-modal__backdrop", onClick (const closeAction) ] []
      , div [ class_ "app-modal__dialog" ]
          [ div [ class_ "app-modal__header" ]
              [ div [ class_ "app-modal__title" ] [ text title ]
              , button
                  [ class_ "btn btn-sm btn-outline-secondary app-modal__close"
                  , onClick (const closeAction)
                  ]
                  [ text "Fermer" ]
              ]
          , div [ class_ "app-modal__body" ] content
          ]
      ]
