module Main where

import Advent.Format (format)
import Data.Maybe (catMaybes)

main :: IO ()
main = do
    input <- catMaybes <$> [format|2024 3 (mul\(%i,%i\)|do\(\)|don't\(\)|~%t)*|]
    print $ eval True True input
    print $ eval True False input

eval :: Bool -> Bool -> [Maybe (Maybe (Int, Int))] -> Int
eval True to ((Just (Just (n, m))) : xs) = n * m + eval True to xs
eval b to ((Just Nothing) : xs) = eval True to xs
eval b to (Nothing : xs) = eval to to xs
eval b to (x:xs) = eval b to xs
eval b to [] = 0
