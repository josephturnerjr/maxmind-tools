module Data.IPLookup
  (IPLookup, fromList, findIP)
  where

import Data.IP
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.List

data IPLookup a = IPLookup {
  values :: V.Vector a,
  starts :: V.Vector IPv4,
  ends :: V.Vector IPv4
} deriving (Show)

fromList :: [IPv4RangeSegment a] -> IPLookup a
fromList is = IPLookup {values = rFL vals, starts = rFL starts, ends = rFL ends} where
  (starts, ends, vals) = foldl' append ([], [], []) is
  append (ss, es, vs) (IPv4RangeSegment (IPv4Range start end) a) = (start:ss, end:es, a:vs)
  rFL = V.reverse . V.fromList

findIP :: IPLookup a -> IPv4 -> Maybe (IPv4RangeSegment a)
findIP ipl ip = do
  idx <- (V.findIndex (\(s, e) -> s <= ip && e >= ip) (V.zip (starts ipl) (ends ipl)))
  let range = IPv4Range ((starts ipl) V.! idx) ((ends ipl) V.! idx)
      value = (values ipl) V.! idx
  return $ IPv4RangeSegment range value  where
    
