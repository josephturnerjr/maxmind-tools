module Data.IPLookup
  (IPLookup, fromList, findIP)
  where

import Data.IP
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as VG
--import Data.List
import Data.Word

data IPLookup a = IPLookup {
  values :: !(V.Vector a),
  ranges :: !(VU.Vector (Word32, Word32))
} deriving (Show)

fromList :: [IPv4RangeSegment a] -> IPLookup a
fromList is = vals `seq` IPLookup {values=vals, ranges=ranges} where
  v = fromList' is
  (starts, ends, vals) = V.unzip3 v
  ranges = V.convert $ V.zip starts ends

fromList' :: [IPv4RangeSegment a] -> V.Vector (Word32, Word32, a)
fromList' = {-# SCC "fromlist" #-}V.fromList . (map toTuple) where
  toTuple (IPv4RangeSegment (IPv4Range (IPv4 start) (IPv4 end)) a) = (start, end, a)

findIP :: IPLookup a -> IPv4 -> Maybe (IPv4RangeSegment a)
findIP ipl (IPv4 ip) = do
  ((start, end), idx) <- lookupIP ip (ranges ipl)
  let range = IPv4Range (IPv4 start) (IPv4 end)
      value = (values ipl) V.! idx
  return $ IPv4RangeSegment range value

lookupIP ip ranges = lookup' 0 ((VU.length ranges) - 1) ip ranges where
  lookup' l h ip ranges | h < l = Nothing
                        | otherwise = case cmp ip pivot of
                                        EQ -> Just (pivot, pivotI)
                                        LT -> lookup' l (pivotI - 1) ip ranges
                                        GT -> lookup' (pivotI + 1) h ip ranges
    where cmp ip (start, end) | ip < start = LT
                              | ip > end = GT
                              | otherwise = EQ
          pivotI = l + ((h - l) `div` 2)
          pivot = ranges VU.! pivotI


{-
type IPLookup a = V.Vector (IPv4, IPv4, a)

fromList :: [IPv4RangeSegment a] -> IPLookup a
fromList = {-# SCC "fromlist" #-}V.fromList . (map toTuple) where
  toTuple (IPv4RangeSegment (IPv4Range start end) a) = (start, end, a)

findIP :: IPLookup a -> IPv4 -> Maybe (IPv4RangeSegment a)
findIP ipl ip = toRS $ V.find (within ip) ipl where
  within ip (start, end, a) = ip >= start && ip <= end
  toRS t = do
    (start, end, a) <- t
    return $ IPv4RangeSegment (IPv4Range start end) a

-}
