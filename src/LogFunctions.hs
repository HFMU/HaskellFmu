module LogFunctions where

import qualified Data.HaskellFMU.Types as T
--data LogType = LogInfo | LogWarning | LogError
--data LogEntry = LogEntry LogType String


logError :: String -> [T.LogEntry]
logError = logX T.LogError

logWarning :: String -> [T.LogEntry]
logWarning = logX T.LogWarning

logInfo :: String -> [T.LogEntry]
logInfo = logX T.LogInfo

logX :: T.LogType -> String -> [T.LogEntry]
logX x y = [LogEntry x y]
