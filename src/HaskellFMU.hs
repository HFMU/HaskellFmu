{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
module HaskellFMU where

import Foreign.C.String
import Foreign.C.Types
import Foreign
import Data.IORef
import Control.Monad
import Control.Applicative
import System.IO.Unsafe
import qualified Data.HashMap.Strict as HM
import Data.List
import Debug.Trace
import qualified Data.HaskellFMU.Types as T
import qualified Data.HaskellFMU.Internal.FMITypes as FMIT
import qualified Data.HaskellFMU.Internal.FMIFunctionTypes as FMIFT
import Data.Maybe
import qualified Control.Monad.Writer as W
import Data.Foldable

foreign import ccall "dynamic" mkFunPtrLogger :: FMIT.CallbackLogger -> FMIT.CompEnvT -> CString -> FMIT.FMIStatus -> CString -> CString -> IO ()

{- |
This function takes two arguments: StablePtr (comp) and Function (f).
It extracts the state from 'comp' and applies 'f' to it.
'f' returns a tuple of state and status.
'comp' is updated with the new state and the status is converted to a 'CInt'
-}
firstFunction :: (StablePtr (IORef (FMIT.FMIComponent a))) -> (FMIT.FMIComponent a -> W.Writer [T.LogEntry] (FMIT.FMIComponent a, T.Status)) -> IO CInt
firstFunction comp f =
  firstFunctionIO comp $ return . f

firstFunctionIO :: (StablePtr (IORef (FMIT.FMIComponent a))) -> (FMIT.FMIComponent a -> IO (W.Writer [T.LogEntry] (FMIT.FMIComponent a, T.Status))) -> IO CInt
firstFunctionIO comp f = do
  state <- getStateImpure comp
  w <- f state
  firstFunctionWriteState comp $ fst . W.runWriter $ w

firstFunctionWriteState :: (StablePtr (IORef (FMIT.FMIComponent a))) -> (FMIT.FMIComponent a, T.Status) -> IO CInt
firstFunctionWriteState comp (state, status) = writeState comp state >> (pure . FMIT.statusToCInt) status

writeState :: StablePtr (IORef a) -> a -> IO ()
writeState ptr state = do
  ioref <- deRefStablePtr ptr
  writeIORef ioref state


reportFatal :: FMIT.FMIComponent a -> (FMIT.FMIComponent a, T.Status)
reportFatal state = (state {FMIT.fcState = FMIT.ERROR}, T.Fatal)

-- ==============================================================
-- =================== STATE CHANGE FUNCTIONS ===================
-- ==============================================================
{- |
Returns a pointer to the state.
C type signature: fmi2String instanceName, fmi2Type fmuType, fmi2String fmuGUID, fmi2String fmuResourceLocation, constfmi2CallbackFunctions* functions, fmi2Boolean visible, fmi2Boolean loggingOn
-}
foreign export ccall fmi2Instantiatee :: CString -> CInt -> CString -> CString -> Ptr FMIT.CallbackFunctions -> CBool -> CBool -> IO (StablePtr (IORef (FMIT.FMIComponent a)))
fmi2Instantiatee :: CString -> CInt -> CString -> CString -> Ptr FMIT.CallbackFunctions -> CBool -> CBool -> IO (StablePtr (IORef (FMIT.FMIComponent a)))
fmi2Instantiatee _ _ guid _ ptrCbFuncs _ _ = do
  -- Extract callback functions
  (cbFuncs :: FMIT.CallbackFunctions) <- peek ptrCbFuncs
  -- Create a test log message
  instanceName :: CString <- newCString "instanceName"
  category :: CString <- newCString "logError"
  msg :: CString <- newCString "HS-Message: Error"
  (mkFunPtrLogger . FMIT.logger $ cbFuncs) nullPtr instanceName (CInt 3) category msg
  state <- getSetupImpure setupVar
  case state of
    Nothing -> putStrLn "NothingCase" >> (newStablePtr =<< newIORef FMIT.FMIComponent {}) -- ERROR SHOULD BE THROWN
    Just s ->
      let retPtr = do
            ioref <- newIORef  FMIT.FMIComponent {fcVars = T.sSVs s, fcDoStep = T.sDoStepFunc s,
                                         fcEndTime = Nothing, fcState = FMIT.Instantiated, fcPeriod = T.sPeriod s, fcRemTime = T.sPeriod s, fcUserState = T.sUserState s}
            newStablePtr ioref in

        do
          guid' <- peekCString guid
          when (T.sGuid s /= guid') . (mkFunPtrLogger . FMIT.logger $ cbFuncs) nullPtr instanceName (CInt 3) category <$> newCString "Invalid GUID"
          retPtr

{- |
Defines the end time.
-}
foreign export ccall fmi2SetupExperiment :: FMIFT.FMISetupExperimentType a
fmi2SetupExperiment :: FMIFT.FMISetupExperimentType a
fmi2SetupExperiment comp _ _ _ _ stopTime =
  let f state = return (state {FMIT.fcEndTime = Just $ realToFrac stopTime}, T.OK) in
    firstFunction comp f


{- |
Changes state to "Initialization Mode"
-}
foreign export ccall fmi2EnterInitializationMode :: FMIFT.FMIEnterInitializationModeType a
fmi2EnterInitializationMode :: FMIFT.FMIEnterInitializationModeType a
fmi2EnterInitializationMode comp =
  firstFunction comp enterInitializationMode

enterInitializationMode :: FMIT.FMIComponent a -> W.Writer [T.LogEntry] (FMIT.FMIComponent a, T.Status)
enterInitializationMode state =
  if FMIT.fcState state == FMIT.Instantiated && (isJust . FMIT.fcEndTime) state
  then
      return (state {FMIT.fcState = FMIT.InitializationMode}, T.OK)
  else
    do
      W.tell [T.LogEntry T.LogError "FMU not instantiated or End Time not defined."]
      return $ reportFatal state
    
{- |
Changes state to slaveInitialized
-}
foreign export ccall fmi2ExitInitializationMode :: FMIFT.FMIExitInitializationModeType a
fmi2ExitInitializationMode :: FMIFT.FMIExitInitializationModeType a
fmi2ExitInitializationMode comp = 
  firstFunction comp exitInitializationMode

exitInitializationMode :: FMIT.FMIComponent a -> W.Writer [T.LogEntry] (FMIT.FMIComponent a, T.Status)
exitInitializationMode state =
  if FMIT.fcState state == FMIT.InitializationMode
  then return (state {FMIT.fcState = FMIT.SlaveInitialized}, T.OK)
  else
    do
      W.tell [T.LogEntry T.LogError "FMU not in Initialization Mode."]
      return $ reportFatal state

{- |
Changes state to terminated
-}
foreign export ccall fmi2Terminate :: FMIFT.FMITerminateType a
fmi2Terminate :: FMIFT.FMITerminateType a
fmi2Terminate comp =
  firstFunction comp terminate

terminate :: FMIT.FMIComponent a -> W.Writer [T.LogEntry] (FMIT.FMIComponent a, T.Status)
terminate state =
  if FMIT.fcState state == FMIT.SlaveInitialized
  then return (state {FMIT.fcState = FMIT.Terminated}, T.OK)
  else do
    W.tell [T.LogEntry T.LogError "FMU not in SlaveInitialized state."]
    return $ reportFatal state

{- |
Releases state.
-}
foreign export ccall fmi2FreeInstance :: FMIFT.FMIFreeInstanceType a
fmi2FreeInstance :: FMIFT.FMIFreeInstanceType a
fmi2FreeInstance comp = do
  freeStablePtr comp
  pure . FMIT.statusToCInt $ T.OK 

-- ==============================================================
-- =================== SET FUNCTIONS ============================
-- ==============================================================
{- |
Sets integer inputs
-}
foreign export ccall fmi2SetInteger :: FMIFT.FMISetIntegerType a
fmi2SetInteger :: FMIFT.FMISetIntegerType a
fmi2SetInteger comp varRefs size varVals =
  let f state = setLogicImpure state varRefs size varVals (T.IntegerVal . fromIntegral) in
    firstFunctionIO comp f

{- |
FFI function to set a real
-}
foreign export ccall fmi2SetReal :: FMIFT.FMISetRealType a
fmi2SetReal :: FMIFT.FMISetRealType a
fmi2SetReal comp varRefs size varVals =
  let f state = setLogicImpure state varRefs size varVals (T.RealVal . realToFrac) in
    firstFunctionIO comp f

setLogicImpure :: Storable b => FMIT.FMIComponent a -> Ptr CUInt -> CSize -> Ptr b -> (b -> T.SVTypeVal) -> IO (W.Writer [T.LogEntry] (FMIT.FMIComponent a, T.Status))
setLogicImpure state refs size vals valConvF =
  do
    refs' :: [Int] <- map fromIntegral <$> peekArray (fromIntegral size) refs
    vals' :: [T.SVTypeVal] <- map valConvF <$> peekArray (fromIntegral size) vals
    return $ setLogic state refs' vals'


setLogic :: FMIT.FMIComponent a -> [Int] -> [T.SVTypeVal] -> W.Writer [T.LogEntry] (FMIT.FMIComponent a,T.Status)
setLogic state@FMIT.FMIComponent {fcVars = vs} vRefs vVals =
  let
    refVals = zip vRefs vVals
    ys' = foldr updateVal vs refVals in
    return (state {FMIT.fcVars = ys'}, T.OK)
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
          _ -> Nothing
      f state = getLogicImpure state varRefs size varVals valToCBool
  in
    firstFunctionIO comp f

foreign export ccall fmi2GetReal :: FMIFT.FMIGetRealType a
fmi2GetReal :: FMIFT.FMIGetRealType a
fmi2GetReal comp varRefs size varVals =
  let valToCDouble (x :: T.SVTypeVal) =
        case x of
          T.RealVal b -> Just . realToFrac $ b
          _ -> Nothing
      f state = getLogicImpure state varRefs size varVals valToCDouble
  in
    firstFunctionIO comp f



getLogicImpure :: Storable b => FMIT.FMIComponent a -> Ptr CUInt -> CSize -> Ptr b -> (T.SVTypeVal -> Maybe b) -> IO (W.Writer [T.LogEntry] (FMIT.FMIComponent a,T.Status))
getLogicImpure state varRefs size varVals toVarValF = do
  varRefs' :: [CUInt] <- peekArray (fromIntegral size) varRefs
  let ((values,status),log) = W.runWriter $ getLogic state (map fromIntegral varRefs') toVarValF
  traverse_ (pokeArray varVals) values
  return $ do
    W.tell log
    return (state,status)

getLogic :: FMIT.FMIComponent b -> [Int] -> (T.SVTypeVal -> Maybe a) -> W.Writer [T.LogEntry] (Maybe [a],T.Status )
getLogic state vRefs valConvF =
  let outputSVs = HM.elems . FMIT.fcVars $ state
      values = getVsFromVRefs outputSVs vRefs valConvF in
    case values of
      Nothing ->
        do
          W.tell $ [T.LogEntry T.LogWarning "Could not find variables to get"]
          return (Nothing, T.Fatal)
      x -> return (x, T.OK)

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
  let
      css' = realToFrac css
      ccp' = realToFrac ccp
      f state = do
        w <- doStepLogic state ccp' css' (toBool ns)
        let ((state',status),log) =  W.runWriter $ w
        case status of
          T.OK -> return $ return (state', T.OK)--writeStateImpure comp state' >> (pure . FMIT.statusToCInt) T.OK
          _ -> return $ return (state, status)

  in
    firstFunctionIO comp f

-- Applies calcDoStep IF
-- /\ end time is defined
-- /\ current communication point + communication step size <= endTime
doStepLogic :: FMIT.FMIComponent a -> T.CurrentCommunicationPoint -> T.CommunicationStepSize -> Bool -> IO (W.Writer [T.LogEntry] (FMIT.FMIComponent a,T.Status))
doStepLogic state ccp css ns =
  case FMIT.fcEndTime state of
    Nothing -> return $ do
      W.tell [T.LogEntry T.LogError "End time has not been defined"]
      return (state, T.Fatal)
    Just endTime ->
      if ccp + css > endTime
      then return $ do
        W.tell [T.LogEntry T.LogError "Step size exceeds the defined end time"]
        return (state, T.Fatal)
      else
        do
          RDS {remTime = rt, vars = vs, status = st, rdsState = rdsS } <- calcDoStep (FMIT.fcDoStep state) (FMIT.fcVars state) (FMIT.fcPeriod state) (FMIT.fcRemTime state) css (FMIT.fcUserState state)
          return $ return (state {FMIT.fcRemTime = rt, FMIT.fcVars=vs, FMIT.fcUserState=rdsS},st)


-- Type representing the Result of Do Steps
data RDS a = RDS {remTime :: Double, vars :: T.SVs, status ::T.Status, rdsState :: T.UserState a}

-- Recursively applies DoStep UNTIL
-- \/ communication step size < remaining time
-- \/ doStep returns status != OK
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

