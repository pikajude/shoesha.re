{-# OPTIONS_GHC -fno-warn-orphans #-}

module Model.Types where

import Data.ISO3166_CountryCodes
import Database.Persist.TH
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
                 deriving (Eq, Ord, Enum, Bounded)

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

instance Read PuddingType where
    readsPrec _ "Almond jelly"              = [(AlmondJelly,"")]
    readsPrec _ "Ashure"                    = [(Ashure,"")]
    readsPrec _ "Asida"                     = [(Asida,"")]
    readsPrec _ "Banana pudding"            = [(BananaPudding,"")]
    readsPrec _ "Bánh chuối"                = [(BánhChuối,"")]
    readsPrec _ "Bebinca"                   = [(Bebinca,"")]
    readsPrec _ "Black pudding"             = [(BlackPudding,"")]
    readsPrec _ "Blancmange"                = [(Blancmange,"")]
    readsPrec _ "Blodpalt"                  = [(Blodpalt,"")]
    readsPrec _ "Bread and butter pudding"  = [(BreadAndButterPudding,"")]
    readsPrec _ "Bread pudding"             = [(BreadPudding,"")]
    readsPrec _ "Brown Betty"               = [(BrownBetty,"")]
    readsPrec _ "Cabinet pudding"           = [(CabinetPudding,"")]
    readsPrec _ "Chè"                       = [(Chè,"")]
    readsPrec _ "Chireta"                   = [(Chireta,"")]
    readsPrec _ "Chocolate pudding"         = [(ChocolatePudding,"")]
    readsPrec _ "Chocolate Biscuit pudding" = [(ChocolateBiscuitPudding,"")]
    readsPrec _ "Christmas pudding"         = [(ChristmasPudding,"")]
    readsPrec _ "Clootie"                   = [(Clootie,"")]
    readsPrec _ "Coconut bread pudding"     = [(CoconutBreadPudding,"")]
    readsPrec _ "Coconut pudding"           = [(CoconutPudding,"")]
    readsPrec _ "Cottage Pudding"           = [(CottagePudding,"")]
    readsPrec _ "Cơm rượu"                  = [(CơmRượu,"")]
    readsPrec _ "Crème caramel"             = [(CrèmeCaramel,"")]
    readsPrec _ "Diplomat pudding"          = [(DiplomatPudding,"")]
    readsPrec _ "Dock pudding"              = [(DockPudding,"")]
    readsPrec _ "Drisheen"                  = [(Drisheen,"")]
    readsPrec _ "Dutch baby pancake"        = [(DutchBabyPancake,"")]
    readsPrec _ "Espasol"                   = [(Espasol,"")]
    readsPrec _ "Eve's pudding"             = [(EvesPudding,"")]
    readsPrec _ "Figgy duff"                = [(FiggyDuff,"")]
    readsPrec _ "Figgy pudding"             = [(FiggyPudding,"")]
    readsPrec _ "Flummadiddle"              = [(Flummadiddle,"")]
    readsPrec _ "Flummery"                  = [(Flummery,"")]
    readsPrec _ "Frumenty"                  = [(Frumenty,"")]
    readsPrec _ "Goody"                     = [(Goody,"")]
    readsPrec _ "Got fan"                   = [(GotFan,"")]
    readsPrec _ "Groaty pudding"            = [(GroatyPudding,"")]
    readsPrec _ "Haggis"                    = [(Haggis,"")]
    readsPrec _ "Hasty pudding"             = [(HastyPudding,"")]
    readsPrec _ "Haupia"                    = [(Haupia,"")]
    readsPrec _ "Jam Roly-Poly"             = [(JamRolyPoly,"")]
    readsPrec _ "Junket"                    = [(Junket,"")]
    readsPrec _ "Kačamak"                   = [(Kačamak,"")]
    readsPrec _ "Keşkül"                    = [(Keşkül,"")]
    readsPrec _ "Kheer"                     = [(Kheer,"")]
    readsPrec _ "Kulolo"                    = [(Kulolo,"")]
    readsPrec _ "Kutia"                     = [(Kutia,"")]
    readsPrec _ "Langevinger pudding"       = [(LangevingerPudding,"")]
    readsPrec _ "Malvern pudding"           = [(MalvernPudding,"")]
    readsPrec _ "Malva Pudding"             = [(MalvaPudding,"")]
    readsPrec _ "Mango pudding"             = [(MangoPudding,"")]
    readsPrec _ "Moin moin"                 = [(MoinMoin,"")]
    readsPrec _ "Panna cotta"               = [(PannaCotta,"")]
    readsPrec _ "Persimmon pudding"         = [(PersimmonPudding,"")]
    readsPrec _ "Pistachio pudding"         = [(PistachioPudding,"")]
    readsPrec _ "Po'e"                      = [(PoE,"")]
    readsPrec _ "Pudding Corn"              = [(PuddingCorn,"")]
    readsPrec _ "Put chai ko"               = [(PutChaiKo,"")]
    readsPrec _ "Queen of Puddings"         = [(QueenOfPuddings,"")]
    readsPrec _ "Rice pudding"              = [(RicePudding,"")]
    readsPrec _ "Rag Pudding"               = [(RagPudding,"")]
    readsPrec _ "Red pudding"               = [(RedPudding,"")]
    readsPrec _ "Rødgrød"                   = [(Rødgrød,"")]
    readsPrec _ "Rượu nếp"                  = [(RượuNếp,"")]
    readsPrec _ "Sago pudding"              = [(SagoPudding,"")]
    readsPrec _ "Scrapple"                  = [(Scrapple,"")]
    readsPrec _ "Spoonbread"                = [(Spoonbread,"")]
    readsPrec _ "Spotted dick"              = [(SpottedDick,"")]
    readsPrec _ "Sticky date pudding"       = [(StickyDatePudding,"")]
    readsPrec _ "Sticky toffee pudding"     = [(StickyToffeePudding,"")]
    readsPrec _ "Summer pudding"            = [(SummerPudding,"")]
    readsPrec _ "Sussex Pond Pudding"       = [(SussexPondPudding,"")]
    readsPrec _ "Tapioca pudding"           = [(TapiocaPudding,"")]
    readsPrec _ "Teurgoule"                 = [(Teurgoule,"")]
    readsPrec _ "Tiết canh"                 = [(TiếtCanh,"")]
    readsPrec _ "Treacle sponge pudding"    = [(TreacleSpongePudding,"")]
    readsPrec _ "Vanilla pudding"           = [(VanillaPudding,"")]
    readsPrec _ "Watalappam"                = [(Watalappam,"")]
    readsPrec _ "White pudding"             = [(WhitePudding,"")]
    readsPrec _ "Yorkshire pudding"         = [(YorkshirePudding,"")]

derivePersistField "PuddingType"

data ShoeQuality = Perfect | Excellent | Acceptable
                 | BeatUp | Garbage
                 deriving (Show, Read)
derivePersistField "ShoeQuality"

data ShoeCategory = Dress | Boot | Sneaker | Sandal | Other
                  deriving (Show, Read)
derivePersistField "ShoeCategory"
