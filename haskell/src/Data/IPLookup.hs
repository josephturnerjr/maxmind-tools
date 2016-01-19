module Data.IPLookup
  (IPLookup, fromList, empty, findIP)
  where

import Data.IP
import qualified Data.Vector as V

type IPLookup a = V.Vector (IPv4RangeSegment a)

fromList :: [IPv4RangeSegment a] -> IPLookup a
fromList = V.fromList

empty :: IPLookup a
empty = V.empty

findIP :: IPLookup a -> IPv4 -> Maybe (IPv4RangeSegment a)
findIP ipl ip = V.find (checkRange ip) ipl where
  checkRange ip (IPv4RangeSegment rng _) = Within == (cmpRange rng ip)
