module Ui.Modal
  ( renderModal
  ) where

import Prelude hiding (div)

import Data.Maybe (Maybe(..))
import Halogen.HTML (HTML, button, div, input, text)
import Halogen.HTML.Events (handler', onClick)
import Halogen.HTML.Properties (IProp, autofocus, ref, type_)
import Ui.Utils (class_)
import Unsafe.Coerce (unsafeCoerce)
import DOM.HTML.Indexed.InputType (InputType(..))
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KeyboardEventTypes
import Data.Newtype (wrap)

renderModal
  :: forall w a i
   . Eq a
  => Maybe a
  -> a
  -> String
  -> Array (HTML w i)
  -> i
  -> i
  -> HTML w i
renderModal activeModal modalToRender title content onCancel onValidate =
  if not (activeModal == Just modalToRender) then text ""
  else
    div
      [ class_ "app-modal"
      , escapeHandler
      ]
      [ div [ class_ "app-modal__backdrop", onClick (const onCancel) ] []
      , div
          [ class_ "app-modal__dialog"
          , escapeHandler
          ]
          [ input
              [ class_ "app-modal__focus"
              , ref (wrap "modal-focus")
              , type_ InputText
              , autofocus true
              , escapeHandler
              ]
          , div [ class_ "app-modal__header" ]
              [ div [ class_ "app-modal__title" ] [ text title ]
              , button
                  [ class_ "btn btn-sm btn-outline-secondary app-modal__close"
                  , onClick (const onCancel)
                  ]
                  [ text "Fermer" ]
              ]
          , div [ class_ "app-modal__body" ] content
          , div [ class_ "app-modal__footer" ]
              [ button
                  [ class_ "btn btn-sm btn-outline-secondary app-modal__cancel"
                  , onClick (const onCancel)
                  ]
                  [ text "Annuler" ]
              , button
                  [ class_ "btn btn-sm btn-primary app-modal__validate"
                  , onClick (const onValidate)
                  ]
                  [ text "Valider" ]
              ]
          ]
      ]
  where
  escapeHandler
    :: forall r
     . IProp
         ( onKeyDown :: KE.KeyboardEvent
         | r
         )
         i
  escapeHandler =
    handler' KeyboardEventTypes.keydown \ev ->
      let
        keyEvent = unsafeCoerce ev :: KE.KeyboardEvent
        key = KE.key keyEvent
      in
        if key == "Escape" then Just onCancel else Nothing
