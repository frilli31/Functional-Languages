{-# OPTIONS_GHC -Wall #-}

module Parse where

import Log 

parseMessage :: String -> LogMessage
parseMessage "" = Unknown ""
parseMessage m = case words m of
    "E":s:t:r -> LogMessage (Error $ read s) (read t) (unwords r)
    "W":t:r -> LogMessage Warning (read t) (unwords r)
    "I":t:r -> LogMessage Info (read t) (unwords r)
    _ -> Unknown m
    
parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)
    