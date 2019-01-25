module Data.HFMU.Internal.FMITypes where

import qualified Data.HFMU.Types as T
import Foreign.C.Types (CInt)
import Foreign.C.String
import Foreign (StablePtr, FunPtr, Ptr, nullPtr)
import Data.IORef
import Foreign.Storable
import Control.Monad (ap)

statusToCInt :: T.Status -> CInt
statusToCInt = fromIntegral . fromEnum

type FMIStatus = CInt
type CompEnvT = Ptr ()
data FMUState = Instantiated
  | InitializationMode
  | SlaveInitialized
  | Terminated
  deriving (Enum, Show)

type FMUStateType a = StablePtr (IORef (FMIComponent a))

type FMIFuncReturn = IO CInt

data FMIComponent x = FMIComponent {fcVars :: T.SVs,
                                  fcDoStep :: T.DoStepFunType x,
                                  fcEndTime :: Maybe Double,
                                  fcState :: FMUState,
                                  fcPeriod :: Double,
                                  fcRemTime :: Double,
                                  fcUserState :: T.UserState x }

type CallbackLogger =
  FunPtr(CompEnvT -> CString ->  FMIStatus -> CString -> CString -> IO ())

type StepFinished = FunPtr(CompEnvT -> FMIStatus -> IO ())

data CallbackFunctions = CallbackFunctions {logger :: CallbackLogger,
                                            allocMem :: Ptr(), -- Ignoring
                                            freeMem :: Ptr (), -- Ignoring
                                            stepFinished :: StepFinished,
                                            compEnv :: CompEnvT}

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
