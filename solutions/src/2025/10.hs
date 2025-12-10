{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PatternSynonyms #-}

module Main where

import Advent.Format (format, format')
import Advent.PQueue qualified as Q
import Data.Array (Array, array, elems, (!), (//))
import Data.Bits (Bits (..))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Word (Word16)
import Advent.Prelude (setAt)
import System.Process (readProcess)

data Config = Config
    { lights :: (Word16, Int)
    , presses :: [[Int]]
    , joltage :: Array Int Int
    } deriving Show

main :: IO ()
main = do
    xs <- [format|2025 10 (\[(\.|#)!*\] (\(%u&(,)\) )*{(%u&,)}%n)*|]
    let configs =
            map
                ( \(as, ys, zs) ->
                    Config
                        -- (array (0, length as - 1) [(i, if x == "#" then On else Off) | (i, x) <- zip [0 ..] as])
                        (foldl' (\acc (i, x) -> if x == "." then setBit acc i else acc) 0 (zip [0 ..] as), length as)
                        ys
                        (array (0, length zs - 1) [(i, x) | (i, x) <- zip [0 ..] zs])
                )
                xs
    print $ sum $ map solve configs
    writeFile "solutions/src/2025/python.txt" $ unlines $ map (\(Config _ presses jolts) -> show (presses <> [elems jolts])) configs
    output <- readProcess "python3" ["./solutions/src/2025/solve.py"] ""
    putStrLn output

-- Part 1

solve :: Config -> Int
solve (Config (originalLight, len) presses _) = go mempty maxBound (Q.singleton 0 (2 ^ len - 1, 0))
  where
    go :: Map Word16 Int -> Int -> Q.PQueue (Word16, Int) -> Int
    go _ best Q.Empty = best
    go seen best (l@(lights, n) Q.:<| xs)
        | best < n = go seen best xs
        | lights == originalLight = go (Map.insert lights (min best n) seen) (min best n) xs
        | Just m <- Map.lookup lights seen, m < n = go seen best xs
        | otherwise = go (Map.insert lights n seen) best (foldr ((\(arr, n) acc -> if n > best then acc else Q.insert n (arr, n) acc) . press l) xs presses)

press :: (Word16, Int) -> [Int] -> (Word16, Int)
press (light, n) xs = (foldl' complementBit light xs, n + 1)
