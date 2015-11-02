module Lib
    ( someFunc
    ) where

import BasePrelude

import Zork

someFunc :: IO ()
someFunc = do
  [zorkDat, userId] <- getArgs
  (p, output) <- startZork zorkDat userId
  print output
  terminate p
