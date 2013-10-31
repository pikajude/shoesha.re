module Model.Types where

import Database.Persist.TH
import Prelude

data ShoeQuality = Perfect | Excellent | Acceptable
                 | BeatUp | Garbage
                 deriving (Show, Read)
derivePersistField "ShoeQuality"

data ShoeCategory = Dress | Boot | Sneaker | Sandal | Other
                  deriving (Show, Read)
derivePersistField "ShoeCategory"
