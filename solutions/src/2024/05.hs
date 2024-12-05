module Main where

import Advent.Format
import Advent.Prelude
import Data.List
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Either
import Data.Maybe (mapMaybe, fromJust)

-- manually removed empty line in input 
main :: IO ()
main = do
    input <- [format|2024 5 (((%i\|%i)|%i&,)%n)*|]
    let (rules', list) = partitionEithers input
    let rules = fmap (\(a,b) -> (b,a)) rules'
    -- print $ sum (middle <$> filter (inOrder mempty rules) list)
    let xs = filter (not . inOrder mempty rules) list
    -- print $ sum $ fmap (middle . fixOrder rules') xs
    print $ sum $ fmap (middle . fmap fst . fix . fixOrder' rules) xs

get :: Int -> [(Int,Int)] -> [Int]
get n = mapMaybe f
  where
    f (a,b) | a == n = Just b
            | otherwise = Nothing

inOrder :: Set Int -> [(Int,Int)] -> [Int] -> Bool
inOrder !seen rules [] = True
inOrder !seen rules (x:xs)
  | x `Set.member` seen = False
  | otherwise = case get x rules of
      ys -> inOrder (Set.fromList ys <> seen) rules xs


middle :: [Int] -> Int
middle xs = let (as,bs) = splitAt (length xs `div` 2) xs in head bs

fixOrder :: [(Int,Int)] -> [Int] -> [Int]
fixOrder rules xs = fromJust $ find (inOrder mempty rules) $ permutations xs

fixOrder' :: [(Int,Int)] -> [Int] -> [(Int,[Int])]
fixOrder' rules xs = [ (x,ys) | x <- xs, let ys = get x rules ]

fix :: [(Int,[Int])] -> [(Int,[Int])]
fix = sortBy f 
  where
    f (a,xs) (b,ys) 
      | a `elem` ys = GT
      | b `elem` xs = LT
      | otherwise = EQ
