{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log
import Data.List (intercalate)
import Data.List.Split (splitOn)

fetchMessageType :: String -> LogMessage
fetchMessageType = createLogMessage . (splitOn " ")

createLogMessage :: [String] -> LogMessage
createLogMessage ("I":timestamp:message) = LogMessage Info (read timestamp) (intercalate " " message)
createLogMessage ("W":timestamp:message) = LogMessage Warning (read timestamp) (intercalate " " message)
createLogMessage ("E":severity:timestamp:message) = LogMessage (Error (read severity)) (read timestamp) (intercalate " " message)
createLogMessage message = Unknown (intercalate " " message)

-- fetchMessageType ('I' : _)            = Info
-- fetchMessageType ('W' : _)            = Warning
-- fetchMessageType ('E' : severity : _) = Error 1
-- fetchMessageType _                  = Unknown

-- parseMessage :: String -> LogMessage
-- parseMessage ('I' : ' ' : timeStamp : ' ' : message) = LogMessage Info timeStamp message
-- parseMessage ('W' : ' ' : timeStamp : ' ' : message) = LogMessage(Warning timeStamp message)
-- parseMessage ('E' : ' ' : severity : ' ' : timeStamp : ' ' : message) = LogMessage((Error severity) timeStamp message)
-- parseMessage message = LogMessage(Unknown message)
