module Data.HaskellFMU.Internal.FMIFunctionTypes where

import Foreign (Ptr)
import Foreign.C.Types (CInt, CUInt, CBool, CDouble, CSize)
import Data.HaskellFMU.Internal.FMITypes

type FMISetupExperimentType a = FMUStateType a -> CBool -> CDouble -> CDouble -> CBool -> CDouble -> FMIFuncReturn

type FMIEnterInitializationModeType a = FMUStateType a -> FMIFuncReturn

type FMIExitInitializationModeType a = FMUStateType a -> FMIFuncReturn

type FMITerminateType a = FMUStateType a -> FMIFuncReturn

type FMIFreeInstanceType a = FMUStateType a -> FMIFuncReturn

type FMISetBooleanType a = FMUStateType a -> Ptr CUInt -> CSize -> Ptr CInt -> FMIFuncReturn

type FMISetIntegerType a = FMUStateType a -> Ptr CUInt -> CSize -> Ptr CInt -> FMIFuncReturn

type FMISetRealType a = FMUStateType a -> Ptr CUInt -> CSize -> Ptr CDouble -> FMIFuncReturn

type FMIGetBooleanType a = FMUStateType a -> Ptr CUInt -> CSize -> Ptr CInt -> FMIFuncReturn

type FMIGetRealType a = FMUStateType a -> Ptr CUInt -> CSize -> Ptr CDouble -> FMIFuncReturn

type FMIGetIntegerType a = FMUStateType a -> Ptr CUInt -> CSize -> Ptr CInt -> FMIFuncReturn


type FMIDoStepType a = FMUStateType a -> CDouble -> CDouble -> CBool -> FMIFuncReturn
