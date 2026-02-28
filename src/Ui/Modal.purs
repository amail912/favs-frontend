module Ui.Modal
  ( renderModal
  ) where

import Prelude hiding (div)

import Agenda.Display (ViewAction(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Halogen.HTML (HTML, button, div, text)
import Halogen.HTML.Events (onClick)
import Ui.Utils (class_)

renderModal
  :: forall w a i
   . Eq a
  => Maybe a
  -> a
  -> String
  -> Array (HTML w i)
  -> HTML w (Either ViewAction i)
renderModal activeModal modalToRender title content =
  if not (activeModal == Just modalToRender) then text ""
  else
    div [ class_ "app-modal" ]
      [ div [ class_ "app-modal__backdrop", onClick (const (Left ViewCloseModal)) ] []
      , div [ class_ "app-modal__dialog" ]
          [ div [ class_ "app-modal__header" ]
              [ div [ class_ "app-modal__title" ] [ text title ]
              , button
                  [ class_ "btn btn-sm btn-outline-secondary app-modal__close"
                  , onClick (const (Left ViewCloseModal))
                  ]
                  [ text "Fermer" ]
              ]
          , div [ class_ "app-modal__body" ] (map (map Right) content)
          ]
      ]
