{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
module HFMU where

import Foreign.C.String
import Foreign.C.Types
import Foreign (Ptr, nullPtr, StablePtr)
import Foreign
import Data.IORef
import Control.Monad
import System.IO.Unsafe
import qualified Data.HashMap.Strict as HM
import Data.List
import Debug.Trace
import qualified Data.HFMU.Types as T
import qualified Data.HFMU.Internal.FMITypes as FMIT
import qualified Data.HFMU.Internal.FMIFunctionTypes as FMIFT


foreign import ccall "dynamic" mkFunPtrLogger :: FMIT.CallbackLogger -> FMIT.CompEnvT -> CString -> FMIT.FMIStatus -> CString -> CString -> IO ()


-- ==============================================================
-- =================== STATE CHANGE FUNCTIONS ===================
-- ==============================================================

-- First function call to the FMU. Should move Setup to FMIComponent
--fmi2Instantiate :: instanceName -> fmuType -> fmuGUID -> fmuResourceLocation -> functions -> visible -> loggingOn
foreign export ccall fmi2Instantiate :: CString -> CInt -> CString -> CString -> Ptr FMIT.CallbackFunctions -> CBool -> CBool -> IO (StablePtr (IORef (FMIT.FMIComponent a)))
fmi2Instantiate :: CString -> CInt -> CString -> CString -> Ptr FMIT.CallbackFunctions -> CBool -> CBool -> IO (StablePtr (IORef (FMIT.FMIComponent a)))
fmi2Instantiate _ _ _ _ ptrCbFuncs _ _ = do
  (cbFuncs :: FMIT.CallbackFunctions) <- peek ptrCbFuncs
  -- logger fmi2ComponentEnv instanceName::String Status::fmi2Status Category::String Message::String
  instanceName :: CString <- newCString "instanceName";
  category :: CString <- newCString "logError";
  msg :: CString <- newCString "HS-Message: Error";
  -- Invoke the function pointer
  (mkFunPtrLogger . FMIT.logger $ cbFuncs) nullPtr instanceName (CInt 3) category msg
  -- Move Setup to FMIComponent
  state <- getSetupImpure setupVar
  case state of
    Nothing -> putStrLn "NothingCase" >> (newStablePtr =<< newIORef FMIT.FMIComponent {}) -- ERROR SHOULD BE THROWN
    Just s -> do
      putStrLn "JustCase"
      ioref <- newIORef  FMIT.FMIComponent {fcVars = T.sSVs s, fcDoStep = T.sDoStepFunc s,
                                         fcEndTime = Nothing, fcState = FMIT.Instantiated, fcPeriod = T.sPeriod s, fcRemTime = T.sPeriod s, fcUserState = T.sUserState s}
      newStablePtr ioref

-- StablePtr IORef FMIComponent -> CBool -> CDouble -> CDouble -> CBool -> CDouble -> IO (Status)
-- fmi2Status fmi2SetupExperiment(fmi2Component c,
-- fmi2Boolean toleranceDefined,
-- fmi2Real tolerance,
-- fmi2Real startTime,
-- fmi2Boolean stopTimeDefined,
-- fmi2Real stopTime);
-- Everything besides stopTime is ignored and stopTime is expected to be defined.
foreign export ccall fmi2SetupExperiment :: FMIFT.FMISetupExperimentType a
fmi2SetupExperiment :: FMIFT.FMISetupExperimentType a
fmi2SetupExperiment comp _ _ _ _ stopTime = do
  state' <- getStateImpure comp
  updStateCalcStatusImpure comp ((state' {FMIT.fcEndTime = Just $ realToFrac stopTime}), T.OK)

foreign export ccall fmi2EnterInitializationMode :: FMIFT.FMIEnterInitializationModeType a
fmi2EnterInitializationMode :: FMIFT.FMIEnterInitializationModeType a
fmi2EnterInitializationMode comp = do
  state <- getStateImpure comp
  case FMIT.fcEndTime state of
    Just _ -> (writeStateImpure comp $ state {FMIT.fcState = FMIT.InitializationMode}) >> pure (FMIT.statusToCInt T.OK)
    Nothing -> pure $ FMIT.statusToCInt T.Fatal

foreign export ccall fmi2ExitInitializationMode :: FMIFT.FMIExitInitializationModeType a
fmi2ExitInitializationMode :: FMIFT.FMIExitInitializationModeType a
fmi2ExitInitializationMode comp = do
  state <- getStateImpure comp
  writeStateImpure comp $ updateState state FMIT.SlaveInitialized
  pure . FMIT.statusToCInt $ T.OK

foreign export ccall fmi2Terminate :: FMIFT.FMITerminateType a
fmi2Terminate :: FMIFT.FMITerminateType a
fmi2Terminate comp = do
  state <- getStateImpure comp
  updStateCalcStatusImpure comp (state {FMIT.fcState = FMIT.Terminated}, T.OK)  

foreign export ccall fmi2FreeInstance :: FMIFT.FMIFreeInstanceType a
fmi2FreeInstance :: FMIFT.FMIFreeInstanceType a
fmi2FreeInstance comp = do
  freeStablePtr comp
  pure . FMIT.statusToCInt $ T.OK 

-- ==============================================================
-- =================== SET FUNCTIONS ============================
-- ==============================================================

foreign export ccall fmi2SetInteger :: FMIFT.FMISetIntegerType a
fmi2SetInteger :: FMIFT.FMISetIntegerType a
fmi2SetInteger comp varRefs size varVals =
  setLogicImpure comp varRefs size varVals (T.IntegerVal . fromIntegral)



foreign export ccall fmi2SetReal :: FMIFT.FMISetRealType a
fmi2SetReal :: FMIFT.FMISetRealType a
fmi2SetReal comp varRefs size varVals =
  setLogicImpure comp varRefs size varVals (T.RealVal . realToFrac)
--  retrieveStateAndCallF comp setLogicImpure2 >>= (\x -> x varRefs size varVals (T.RealVal . realToFrac))

setLogicImpure :: Storable b => FMIT.FMUStateType a -> Ptr CUInt -> CSize -> Ptr b -> (b -> T.SVTypeVal) -> IO CInt
setLogicImpure comp varRefs size varVals varValConvF =
  do
    state <- getStateImpure comp
    varRefs' :: [CUInt] <- peekArray (fromIntegral size) varRefs
    varVals' <- peekArray (fromIntegral size) varVals
    let
      varVals'' :: [T.SVTypeVal] = map varValConvF varVals'
      varRefs'' = map fromIntegral varRefs'
      stateStatus = setLogic state varRefs'' varVals''
      in
      updStateCalcStatusImpure comp stateStatus

--setLogicImpure2 :: Storable b => FMIT.FMIComponent a -> Ptr CUInt -> CSize -> Ptr b -> (b -> T.SVTypeVal) -> IO CInt
--setLogicImpure2 state varRefs size varVals varValConvF =
--  do
--    varRefs' :: [CUInt] <- peekArray (fromIntegral size) varRefs
--    varVals' <- peekArray (fromIntegral size) varVals
--    let
--      varVals'' :: [T.SVTypeVal] = map varValConvF varVals'
--      varRefs'' = map fromIntegral varRefs'
--      stateStatus = setLogic state varRefs'' varVals''
--      in
--      return 5
      
-- POssible better function. Retrieves the state from component
purePreComponentHandler :: FMIT.FMUStateType a -> (FMIT.FMIComponent a -> b) -> IO b
purePreComponentHandler comp f = do
  state <- getStateImpure comp
  return $ f state

-- POssible better function. Retrieves the state from component
impurePreComponentHandler :: FMIT.FMUStateType a -> (FMIT.FMIComponent a -> IO b) -> IO b
impurePreComponentHandler comp f = do
  state <- getStateImpure comp
  f state

setLogic :: FMIT.FMIComponent a -> [Int] -> [T.SVTypeVal] -> (FMIT.FMIComponent a,T.Status)
setLogic state@FMIT.FMIComponent {fcVars = vs} vRefs vVals =
  let
    refVals = zip vRefs vVals
    ys' = foldr updateVal vs refVals in
    (state {FMIT.fcVars = ys'}, T.OK)
  where
    updateVal :: (Int, T.SVTypeVal) -> T.SVs -> T.SVs
    updateVal (valRef, valVal) hm = HM.map updateValWithValRef hm
      where
        updateValWithValRef :: T.SV -> T.SV
        updateValWithValRef x = if T.svRef x == valRef then x {T.svVal = valVal} else x

    

-- ==============================================================
-- =================== GET FUNCTIONS ============================
-- ==============================================================

foreign export ccall fmi2GetBoolean :: FMIFT.FMIGetBooleanType a
fmi2GetBoolean :: FMIFT.FMIGetBooleanType a
fmi2GetBoolean comp varRefs size varVals =
  let valToCBool (x :: T.SVTypeVal) =
        case x of
          T.BooleanVal b -> Just . fromBool $ b
          _ -> Nothing in
    getLogicImpure comp varRefs size varVals valToCBool

foreign export ccall fmi2GetReal :: FMIFT.FMIGetRealType a
fmi2GetReal :: FMIFT.FMIGetRealType a
fmi2GetReal comp varRefs size varVals =
  let valToCDouble (x :: T.SVTypeVal) =
        case x of
          T.RealVal b -> Just . realToFrac $ b
          _ -> Nothing in
    getLogicImpure comp varRefs size varVals valToCDouble



getLogicImpure :: Storable a => FMIT.FMUStateType b -> Ptr CUInt -> CSize -> Ptr a -> (T.SVTypeVal -> Maybe a) -> IO CInt
getLogicImpure comp varRefs size varVals toVarValF = do
  state <- getStateImpure comp
  varRefs' :: [CUInt] <- peekArray (fromIntegral size) varRefs
  let (values,status) = getLogic state (map fromIntegral varRefs') toVarValF in
    case values of
      Nothing -> pure . FMIT.statusToCInt $ status
      Just values' -> pokeArray varVals values' >> (pure . FMIT.statusToCInt) status

getLogic :: FMIT.FMIComponent b -> [Int] -> (T.SVTypeVal -> Maybe a) -> (Maybe [a],T.Status )
getLogic state vRefs valConvF =
  let outputSVs = HM.elems . FMIT.fcVars $ state
      values = getVsFromVRefs outputSVs vRefs valConvF in
    case values of
      Nothing -> (Nothing, T.Fatal)
      x -> (x, T.OK)

getVsFromVRefs :: [T.SV] -> [Int] -> (T.SVTypeVal -> Maybe a) -> Maybe [a]
getVsFromVRefs ps refs f = traverse (\x -> f =<< findValWithRef x ps) refs
  where
    findValWithRef :: Int -> [T.SV] -> Maybe T.SVTypeVal
    findValWithRef ref ps' = T.svVal <$> find (\(x :: T.SV) -> T.svRef x == ref) ps'


-- ==============================================================
-- =================== DoStep FUNCTION ==========================
-- ==============================================================


-- fmi2Component comp,
-- fmi2Real currentCommunicationPoint,
-- fmi2Real communicationStepSize,
-- fmi2Real noSetFMUStatePriorToCurrentPoint
-- 
foreign export ccall fmi2DoStep :: FMIFT.FMIDoStepType a
fmi2DoStep :: FMIFT.FMIDoStepType a
fmi2DoStep comp ccp css ns =
  do
    state <- getStateImpure comp
    let
      css' = realToFrac css
      ccp' = realToFrac ccp in
      do
        (state', status) <- doStepLogic state ccp' css' (toBool ns)
        case status of
          T.OK -> writeStateImpure comp state' >> (pure . FMIT.statusToCInt) T.OK
          _ -> (pure . FMIT.statusToCInt) status


doStepLogic :: FMIT.FMIComponent a -> T.CurrentCommunicationPoint -> T.CommunicationStepSize -> Bool -> IO (FMIT.FMIComponent a,T.Status)
doStepLogic state ccp css ns =
  case FMIT.fcEndTime state of
    Nothing -> return (state, T.Fatal)
    Just endTime ->
      if ccp + css > endTime
      then return (state, T.Fatal)
      else
        do
          RDS {remTime = rt, vars = vs, status = st, rdsState = rdsS } <- calcDoStep (FMIT.fcDoStep state) (FMIT.fcVars state) (FMIT.fcPeriod state) (FMIT.fcRemTime state) css (FMIT.fcUserState state)
          return (state {FMIT.fcRemTime = rt, FMIT.fcVars=vs, FMIT.fcUserState=rdsS},st)

data RDS a = RDS {remTime :: Double, vars :: T.SVs, status ::T.Status, rdsState :: T.UserState a}

calcDoStep :: T.DoStepFunType a -> T.SVs -> T.Period -> Double -> T.CommunicationStepSize -> T.UserState a -> IO (RDS a)
calcDoStep doStepF svs period remTime css us
  | css < remTime = return RDS {remTime = remTime - css, vars = svs, status = T.OK, rdsState = us}
  | otherwise = do
      T.DoStepResult {T.dsrStatus = st, T.dsrSvs = svs', T.dsrState = sta} <- doStepF svs us
      case st of
        T.OK -> calcDoStep doStepF svs' period period (css-remTime) sta
        _ -> return RDS {remTime = remTime, vars=svs', status=st, rdsState=sta}

-- ==============================================================
-- =================== SETUP FUNCTIONs ==========================
-- ==============================================================

-- Invoked from FMU
setup :: T.Setup a -> IO ()
setup = storeSetupImpure

-- Functions related to the user defined doStep function
{-# NOINLINE setupVar #-}
setupVar :: IORef (Maybe (T.Setup a))
setupVar = unsafePerformIO $ newIORef Nothing

storeSetupImpure :: T.Setup a -> IO ()
storeSetupImpure = writeIORef setupVar . Just

getSetupImpure :: IORef (Maybe (T.Setup a)) -> IO (Maybe (T.Setup a))
getSetupImpure = readIORef


-- ==============================================================
-- =================== STATE FUNCTIONs ==========================
-- ==============================================================

-- Retrieves state
getStateImpure :: StablePtr (IORef a) -> IO a
getStateImpure = deRefStablePtr >=> readIORef

-- Stores state
writeStateImpure :: StablePtr (IORef a) -> a -> IO ()
writeStateImpure ptr state = do
  ioref <- deRefStablePtr ptr
  writeIORef ioref state



updStateCalcStatusImpure :: FMIT.FMUStateType a -> (FMIT.FMIComponent a,T.Status) -> IO CInt
updStateCalcStatusImpure comp (state, status) = writeStateImpure comp state >> (pure . FMIT.statusToCInt) status

-- ==============================================================
-- =================== UTIL FUNCTIONs ===========================
-- ==============================================================

updateState :: FMIT.FMIComponent a -> FMIT.FMUState -> FMIT.FMIComponent a
updateState c s = c {FMIT.fcState = s}

updateInputs :: FMIT.FMIComponent a -> T.SVs -> FMIT.FMIComponent a
updateInputs c i = c {FMIT.fcVars = i}

