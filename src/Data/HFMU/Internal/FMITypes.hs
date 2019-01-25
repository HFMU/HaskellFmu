{-# LINE 1 "src/Data/HFMU/Internal/FMITypes.hsc" #-}
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




instance Storable CallbackFunctions where
  sizeOf _ = ((40))
{-# LINE 50 "src/Data/HFMU/Internal/FMITypes.hsc" #-}
  alignment _ = (8)
{-# LINE 51 "src/Data/HFMU/Internal/FMITypes.hsc" #-}
  peek p = return CallbackFunctions
    `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 0) p)
{-# LINE 53 "src/Data/HFMU/Internal/FMITypes.hsc" #-}
    `ap` return nullPtr
    `ap` return nullPtr
    `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 24) p)
{-# LINE 56 "src/Data/HFMU/Internal/FMITypes.hsc" #-}
    `ap`  ((\hsc_ptr -> peekByteOff hsc_ptr 32) p)
{-# LINE 57 "src/Data/HFMU/Internal/FMITypes.hsc" #-}
