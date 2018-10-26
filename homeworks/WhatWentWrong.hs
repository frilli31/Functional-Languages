{-# OPTIONS_GHC -Wall #-}

module WhatWentWrong where

import Log 
import Prelude

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t

insert message Leaf = Node Leaf message Leaf
insert new@(LogMessage _ newTS _) (Node left old@(LogMessage _ ts _) right) =
  if newTS > ts
    then Node left old (insert new right)
    else Node (insert new left) old right
insert _ tree = error (show tree)

build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf                      = []
inOrder (Node left message right) = inOrder left ++ [message] ++ inOrder right

isWrong :: LogMessage -> Bool
isWrong (LogMessage (Error severity) _ _) = severity > 50
isWrong _                                 = False

toString :: LogMessage -> String
toString (LogMessage _ _ message) = message
toString (Unknown message)        = message

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map toString . filter isWrong . inOrder . build
