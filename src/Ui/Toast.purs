module Ui.Toast (render) where

import Prelude hiding (div)

import Halogen.HTML (HTML, button, div, text)
import Halogen.HTML.Events (onClick)
import Ui.Utils (class_)

render :: forall w i. String -> String -> i -> HTML w i
render toneClass message dismissAction =
  div [ class_ ("app-toast " <> toneClass) ]
    [ div [ class_ "app-toast__message" ] [ text message ]
    , button [ class_ "btn btn-sm btn-light app-toast__dismiss", onClick (const dismissAction) ] [ text "OK" ]
    ]
