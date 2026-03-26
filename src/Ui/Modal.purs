module Ui.Modal
  ( renderModal
  , renderBottomSheet
  ) where

import Prelude hiding (div)

import Data.Maybe (Maybe(..))
import Halogen.HTML (HTML, button, div, input, text)
import Halogen.HTML.Events (handler', onClick)
import Halogen.HTML.Properties (IProp, autofocus, ref, type_)
import Ui.Utils (class_)
import DOM.HTML.Indexed.InputType (InputType(..))
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KeyboardEventTypes
import Data.Newtype (wrap)

renderModal
  :: forall w i
   . String
  -> Array (HTML w i)
  -> i
  -> i
  -> HTML w i
renderModal title content onCancel onValidate =
  renderSurface "app-modal__dialog" title content onCancel (Just onValidate)

renderBottomSheet
  :: forall w i
   . String
  -> Array (HTML w i)
  -> i
  -> HTML w i
renderBottomSheet title content onCancel =
  renderSurface "app-bottom-sheet__dialog" title content onCancel Nothing

renderSurface
  :: forall w i
   . String
  -> String
  -> Array (HTML w i)
  -> i
  -> Maybe i
  -> HTML w i
renderSurface dialogClass title content onCancel onValidate =
  div
    [ class_ "app-modal"
    , escapeHandler
    ]
    [ div [ class_ "app-modal__backdrop", onClick (const onCancel) ] []
    , div
        [ class_ dialogClass
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
        , case onValidate of
            Nothing -> text ""
            Just onValidate' ->
              div [ class_ "app-modal__footer" ]
                [ button
                    [ class_ "btn btn-sm btn-outline-secondary app-modal__cancel"
                    , onClick (const onCancel)
                    ]
                    [ text "Annuler" ]
                , button
                    [ class_ "btn btn-sm btn-primary app-modal__validate"
                    , onClick (const onValidate')
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
      case KE.fromEvent ev of
        Nothing -> Nothing
        Just keyEvent ->
          if KE.key keyEvent == "Escape" then Just onCancel else Nothing
