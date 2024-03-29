module Model where

import Prelude
import Yesod
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Fixed
import Data.ISO3166_CountryCodes
import Database.Persist.Quasi
import Data.Typeable (Typeable)
import Model.Types

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")
