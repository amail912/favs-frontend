module Api.AgendaContract
  ( Method(..)
  , listPath
  , createPath
  , updatePath
  , updateMethod
  , validatePath
  ) where

import Prelude

data Method = GET | POST | PATCH | DELETE

derive instance methodEq :: Eq Method

instance methodShow :: Show Method where
  show GET = "GET"
  show POST = "POST"
  show PATCH = "PATCH"
  show DELETE = "DELETE"

listPath :: String
listPath = "/api/v1/calendar-items"

createPath :: String
createPath = "/api/v1/calendar-items"

updatePath :: String -> String
updatePath _ = "/api/v1/calendar-items"

updateMethod :: Method
updateMethod = POST

validatePath :: String -> String
validatePath itemId = "/api/v1/calendar-items/" <> itemId <> "/validate"
