module LogFunctions where

import qualified Data.HaskellFMU.Types as T
import qualified Data.HaskellFMU.Internal.FMITypes as FMIT
import Foreign.C.String
import Foreign.C.Types
import Foreign

logError :: String -> [T.LogEntry]
logError = logX T.LogError

logWarning :: String -> [T.LogEntry]
logWarning = logX T.LogWarning

logInfo :: String -> [T.LogEntry]
logInfo = logX T.LogInfo

logX :: T.LogType -> String -> [T.LogEntry]
logX x y = [T.LogEntry x y]

data LogCategory2 = Ok | Warning | Error | Fatal

showLogCategory :: LogCategory2 -> String
showLogCategory Ok = "OK"
showLogCategory Warning = "Warning"
showLogCategory Error = "Error"
showLogCategory Fatal = "Fatal"

logCategoryToInt :: LogCategory2 -> Int32
logCategoryToInt Ok = 0
logCategoryToInt Warning = 1
logCategoryToInt Error = 3 -- Skipping fmi2Discard
logCategoryToInt Fatal = 4

type LoggingFunctionType = LogCategory2 -> String -> IO ()

foreign import ccall "dynamic" mkFunPtrLogger :: FMIT.CallbackLogger -> FMIT.CompEnvT -> CString -> FMIT.FMIStatus -> CString -> CString -> IO ()

createLoggingFunction :: FMIT.CallbackFunctions  -> String -> LoggingFunctionType
createLoggingFunction cbFuncs instanceName logCat msg = 
	let 
		loggerFunction = FMIT.logger cbFuncs
		loggerFunctionPtr = mkFunPtrLogger loggerFunction
	in
		do
			instanceName_ <- newCString instanceName
			logCategory <- newCString  (showLogCategory logCat)
			msg_ <- newCString msg
			loggerFunctionPtr nullPtr instanceName_ (CInt (logCategoryToInt logCat)) logCategory msg_