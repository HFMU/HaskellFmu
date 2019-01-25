
module HSFMIInterface where
import Foreign.Storable
import Foreign (Ptr, nullPtr, FunPtr, StablePtr)
import Foreign.C.Types (CInt)
import Foreign.C.String
import Control.Monad (ap)

#include "fmi2Functions.h"

instance Storable CallbackFunctions where
  sizeOf _ = (#size fmi2CallbackFunctions)
  alignment _ = (#alignment fmi2CallbackFunctions)
  peek p = return CallbackFunctions
    `ap` (#{peek fmi2CallbackFunctions, logger} p)
    `ap` return nullPtr
    `ap` return nullPtr
    `ap` (#{peek fmi2CallbackFunctions, stepFinished} p)
    `ap`  (#{peek fmi2CallbackFunctions, componentEnvironment} p)
