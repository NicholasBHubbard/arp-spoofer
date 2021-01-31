{-# LANGUAGE LambdaCase #-}

module AddressCaching where

import           Control.Monad   (replicateM)
import           Data.List.Split (splitOn)
import qualified Data.Map        as Map
import           System.Process  (readProcess)
import qualified Text.Trifecta   as P

type AddressMap = Map.Map IpAddr MacAddr
type IpAddr  = String
type MacAddr = String

                 {------------------------------------}
                 {-        Produce Address Map       -}
                 {------------------------------------}

makeIpMacMap :: IO AddressMap
makeIpMacMap = do

  arpscan <- readProcess "/usr/bin/sudo" ["arp-scan","--localnet"] []

  let arpscan'  = splitOn "\n" arpscan
      parse     = P.parseString parseArpScanLine mempty
      p         = \case {P.Success _ -> True; _ -> False}
      successes = filter p $ map parse arpscan'
      results   = (\(P.Success a) -> a) $ sequence successes
      resultMap = Map.fromList results
  pure $ resultMap
-- successes :: [P.Result (IpAddr, MacAddr)]

                 {------------------------------------}
                 {-         arp-scan Parsing         -}
                 {------------------------------------}

parseArpScanLine :: P.Parser (IpAddr, MacAddr)
parseArpScanLine = do
  ip  <- parseIp
  mac <- parseMac
  pure (ip, mac)

-- 192.168.1.10
parseIp :: P.Parser IpAddr
parseIp = do
  octDot3 <- replicateM 3
               ( do; oct <- fmap show P.integer;
                     dot <- P.string ".";
                     pure $ oct <> dot )
  lastOct <- fmap show P.integer
  pure $ (concat octDot3) <> lastOct

parseMac :: P.Parser MacAddr
parseMac = do
  sextSemi5 <- replicateM 5
                ( do; sext  <- replicateM 2 (P.anyChar >>= \c -> pure c);
                      semi <- P.string ":";
                      pure $ sext <> semi )
  lastSext  <- replicateM 2 (P.anyChar >>= \c -> pure c)
  pure $ (concat sextSemi5) <> lastSext
