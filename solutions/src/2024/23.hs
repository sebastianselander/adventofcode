module Main where

import Advent.Format (format)
import Data.Algorithm.MaximalCliques (getMaximalCliques)
import Data.List (intercalate, maximumBy)
import Data.List.Extra (nubOrd)
import Data.Map.Strict ((!), insertWith, keys)
import Data.Ord (comparing)
import Data.Set (fromList)

main :: IO ()
main = do
    g <-
        foldr (\(l, r) acc -> insertWith (++) l [r] $ insertWith (++) r [l] acc) mempty
            <$> [format|2024 23 (%s-%s%n)*|]
    let three k =
            [ fromList [k, k', k'']
            | let neighbors = (g !)
            , k'@(x : _) <- neighbors k
            , k''@(y : _) <- neighbors k'
            , k'''@(z : _) <- neighbors k''
            , k''' == k
            , 't' `elem` [x, y, z]
            ]
    print $ length $ nubOrd $ concatMap three (keys g)
    putStrLn $
        intercalate "," $
                maximumBy (comparing length) $
                    getMaximalCliques (\l -> elem l . (g !)) (keys g)
