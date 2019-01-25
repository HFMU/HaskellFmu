module HMOperations where

import Data.HFMU.Types
import qualified Data.HashMap.Strict as HM

adjustPortVal :: SVs -> String -> SVTypeVal -> SVs
adjustPortVal sv name v = HM.adjust (\p -> p {svVal = v}) name sv


getPortVal :: SVs -> String -> Maybe SVTypeVal
getPortVal sv name = svVal <$> HM.lookup name sv


