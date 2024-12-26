{-# LANGUAGE TemplateHaskell #-}

module Main where

import Advent.Format (format, intro)
import Advent.Prelude (fromDigits)
import Advent.Queue (Queue (..), (|>))
import Advent.Queue qualified as Queue
import Data.Bits (Bits (xor, (.&.), (.|.)))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

data Op = Op_AND | Op_XOR | Op_OR

intro

main :: IO ()
main = do
    (input, moves) <- [format|2024 24 (%s: %u%n)*%n(%s @Op %s -> %s%n)*|]
    print $ solve input moves
    -- done by hand using graphviz
    print "cnk,mps,msq,qwf,vhm,z14,z27,z39"

solve :: [(String, Int)] -> [(String, Op, String, String)] -> Int
solve vs mvs =
    fromDigits
        2
        [ v | ('z' : _, v) <- Map.toDescList (part1 (Map.fromList vs) (Queue.fromList mvs))
        ]

part1 :: Map String Int -> Queue (String, Op, String, String) -> Map String Int
part1 m Queue.Empty = m
part1 m (x@(l, op, r, result) :<| xs)
    | Just lv <- Map.lookup l m
    , Just rv <- Map.lookup r m =
        part1 (Map.insert result (fop op lv rv) m) xs
    | otherwise = part1 m (xs |> x)
  where
    fop Op_AND = (.&.)
    fop Op_XOR = xor
    fop Op_OR = (.|.)
