module Main where

import Advent.Format (format)
import Advent.Prelude (count)
import Data.Algorithm.MaximalCliques
import Data.List (intercalate, maximumBy, sort)
import Data.List.Extra (nubOrd)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Ord (comparing)

main :: IO ()
main = do
    input <- [format|2024 23 (%s-%s%n)*|]
    let g = foldr (\(l,r) acc -> Map.insertWith (++) l [r] $ Map.insertWith (++) r [l] acc) mempty input
        three :: String -> [[String]]
        three k = nubOrd [sort [k, ns, ns'] | ns <- neighbors k, ns' <- neighbors ns, ns'' <- neighbors ns', ns'' == k]
          where
            neighbors x = g Map.! x
        maxClique = maximumBy (comparing length) (getMaximalCliques (\l r -> r `elem` g Map.! l) (Map.keys g))
    print $ count True $ fmap (any (\(x : _) -> x == 't')) $ nubOrd $ concat [sort (three k) | k <- Map.keys g]
    putStrLn $ intercalate "," $ sort maxClique


