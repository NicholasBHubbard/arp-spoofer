module AddressCaching where

import qualified Data.Map       as Map
import           System.Process
import qualified Text.Trifecta  as P

type AddressMap = Map.Map IpAddr MacAddr

type IpAddr  = String
type MacAddr = String

populateCache :: IO ()
populateCache = do
  str <- readProcess "pwd" [] []
  print str

--parseTcpDumpWhoHas ::

-- 15:41:13.030665 ARP, Request who-has 192.168.1.153 tell 192.168.1.1, length 46
-- 15:41:13.030692 ARP, Reply 192.168.1.153 is-at 80:fa:5b:7b:2f:33, length 28

