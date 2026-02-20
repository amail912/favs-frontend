module Ui.Utils (class_) where

import Prelude

import Data.Newtype (wrap)
import Data.String (Pattern(..), split)
import Halogen.HTML.Properties (IProp, classes)


class_ :: forall r i. String -> IProp (class :: String | r) i
class_ = split (Pattern " ") >>> (map wrap) >>> classes
