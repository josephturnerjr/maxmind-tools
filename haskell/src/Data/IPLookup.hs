module Data.IPLookup
  (IPLookup, fromList, findIP)
  where

import Data.IP
import qualified Data.Vector as V

type IPLookup a = V.Vector (IPv4, IPv4, a)

fromList :: [IPv4RangeSegment a] -> IPLookup a
fromList = V.fromList . (map toTuple) where
  toTuple (IPv4RangeSegment (IPv4Range start end) a) = (start, end, a)

findIP :: IPLookup a -> IPv4 -> Maybe (IPv4RangeSegment a)
findIP ipl ip = do
  (start, end, a) <- V.find (checkRange ip) ipl 
  return $ IPv4RangeSegment (IPv4Range start end) a where
    checkRange ip (start, end, a) = ip >= start && ip <= end
