module Data.IPLookup
  ()
  where

type IPLookup = S.Seq

fromList :: [IPv4RangeSegment a] -> IPLookup a
fromList = S.fromList

empty :: IPLookup a
empty = S.empty

