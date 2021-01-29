module Main where

import           AddressCaching
import           Control.Concurrent (forkIO)

main :: IO ()
main = populateCache
--  forkIO causeNetworkDelays
--  spoof

