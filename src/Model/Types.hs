{-# OPTIONS_GHC -fno-warn-orphans #-}

module Model.Types where

import Control.Arrow
import Data.ISO3166_CountryCodes
import Database.Persist.TH
import Data.Text (Text, pack)
import Prelude

derivePersistField "CountryCode"

data PuddingType = AlmondJelly | Ashure | Asida | BananaPudding | BánhChuối
                 | Bebinca | BlackPudding | Blancmange | Blodpalt | BreadAndButterPudding
                 | BreadPudding | BrownBetty | CabinetPudding | Chè | Chireta
                 | ChocolatePudding | ChocolateBiscuitPudding | ChristmasPudding
                 | Clootie | CoconutBreadPudding | CoconutPudding | CottagePudding
                 | CơmRượu | CrèmeCaramel | DiplomatPudding | DockPudding | Drisheen
                 | DutchBabyPancake | Espasol | EvesPudding | FiggyDuff | FiggyPudding
                 | Flummadiddle | Flummery | Frumenty | Goody | GotFan | GroatyPudding
                 | Haggis | HastyPudding | Haupia | JamRolyPoly | Junket | Kačamak
                 | Keşkül | Kheer | Kulolo | Kutia | LangevingerPudding | MalvernPudding
                 | MalvaPudding | MangoPudding | MoinMoin | PannaCotta | PersimmonPudding
                 | PistachioPudding | PoE | PuddingCorn | PutChaiKo | QueenOfPuddings
                 | RicePudding | RagPudding | RedPudding | Rødgrød | RượuNếp
                 | SagoPudding | Scrapple | Spoonbread | SpottedDick | StickyDatePudding
                 | StickyToffeePudding | SummerPudding | SussexPondPudding | TapiocaPudding
                 | Teurgoule | TiếtCanh | TreacleSpongePudding | VanillaPudding
                 | Watalappam | WhitePudding | YorkshirePudding
                 deriving (Eq, Read, Ord, Enum, Bounded)

instance Show PuddingType where
    show AlmondJelly             = "Almond jelly"
    show Ashure                  = "Ashure"
    show Asida                   = "Asida"
    show BananaPudding           = "Banana pudding"
    show BánhChuối               = "Bánh chuối"
    show Bebinca                 = "Bebinca"
    show BlackPudding            = "Black pudding"
    show Blancmange              = "Blancmange"
    show Blodpalt                = "Blodpalt"
    show BreadAndButterPudding   = "Bread and butter pudding"
    show BreadPudding            = "Bread pudding"
    show BrownBetty              = "Brown Betty"
    show CabinetPudding          = "Cabinet pudding"
    show Chè                     = "Chè"
    show Chireta                 = "Chireta"
    show ChocolatePudding        = "Chocolate pudding"
    show ChocolateBiscuitPudding = "Chocolate Biscuit pudding"
    show ChristmasPudding        = "Christmas pudding"
    show Clootie                 = "Clootie"
    show CoconutBreadPudding     = "Coconut bread pudding"
    show CoconutPudding          = "Coconut pudding"
    show CottagePudding          = "Cottage Pudding"
    show CơmRượu                 = "Cơm rượu"
    show CrèmeCaramel            = "Crème caramel"
    show DiplomatPudding         = "Diplomat pudding"
    show DockPudding             = "Dock pudding"
    show Drisheen                = "Drisheen"
    show DutchBabyPancake        = "Dutch baby pancake"
    show Espasol                 = "Espasol"
    show EvesPudding             = "Eve's pudding"
    show FiggyDuff               = "Figgy duff"
    show FiggyPudding            = "Figgy pudding"
    show Flummadiddle            = "Flummadiddle"
    show Flummery                = "Flummery"
    show Frumenty                = "Frumenty"
    show Goody                   = "Goody"
    show GotFan                  = "Got fan"
    show GroatyPudding           = "Groaty pudding"
    show Haggis                  = "Haggis"
    show HastyPudding            = "Hasty pudding"
    show Haupia                  = "Haupia"
    show JamRolyPoly             = "Jam Roly-Poly"
    show Junket                  = "Junket"
    show Kačamak                 = "Kačamak"
    show Keşkül                  = "Keşkül"
    show Kheer                   = "Kheer"
    show Kulolo                  = "Kulolo"
    show Kutia                   = "Kutia"
    show LangevingerPudding      = "Langevinger pudding"
    show MalvernPudding          = "Malvern pudding"
    show MalvaPudding            = "Malva Pudding"
    show MangoPudding            = "Mango pudding"
    show MoinMoin                = "Moin moin"
    show PannaCotta              = "Panna cotta"
    show PersimmonPudding        = "Persimmon pudding"
    show PistachioPudding        = "Pistachio pudding"
    show PoE                     = "Po'e"
    show PuddingCorn             = "Pudding Corn"
    show PutChaiKo               = "Put chai ko"
    show QueenOfPuddings         = "Queen of Puddings"
    show RicePudding             = "Rice pudding"
    show RagPudding              = "Rag Pudding"
    show RedPudding              = "Red pudding"
    show Rødgrød                 = "Rødgrød"
    show RượuNếp                 = "Rượu nếp"
    show SagoPudding             = "Sago pudding"
    show Scrapple                = "Scrapple"
    show Spoonbread              = "Spoonbread"
    show SpottedDick             = "Spotted dick"
    show StickyDatePudding       = "Sticky date pudding"
    show StickyToffeePudding     = "Sticky toffee pudding"
    show SummerPudding           = "Summer pudding"
    show SussexPondPudding       = "Sussex Pond Pudding"
    show TapiocaPudding          = "Tapioca pudding"
    show Teurgoule               = "Teurgoule"
    show TiếtCanh                = "Tiết canh"
    show TreacleSpongePudding    = "Treacle sponge pudding"
    show VanillaPudding          = "Vanilla pudding"
    show Watalappam              = "Watalappam"
    show WhitePudding            = "White pudding"
    show YorkshirePudding        = "Yorkshire pudding"

derivePersistField "PuddingType"

data ShoeQuality = Perfect | Excellent | Acceptable
                 | BeatUp | Garbage
                 deriving (Show, Read)
derivePersistField "ShoeQuality"

data ShoeCategory = Dress | Boot | Sneaker | Sandal | Other
                  deriving (Show, Read)
derivePersistField "ShoeCategory"
