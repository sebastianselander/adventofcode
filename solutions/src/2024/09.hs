module Main where

import Advent.Format ( format )
import Data.List (dropWhileEnd)
import Data.Maybe (isNothing, fromJust, mapMaybe)
import Data.List.Extra (dropEnd)
import Data.Sequence qualified as Seq
import Data.Sequence (Seq(..))
import Data.Foldable ( Foldable(toList) )

main :: IO ()
main = do
    input <- [format|2024 9 (%d*)%n|]
    print $ sum $ zipWith (*) [0 ..] $ smaller $ checksum $ compact 0 input
    print $ sum $ mapMaybe (\(l,r) -> fmap (l*) r) $ zip [0 ..] $ checksum $ toList $ move $ Seq.fromList $ compact 0 input

data Block = N Int Int | Free Int
    deriving Show

move :: Seq Block -> Seq Block
move Seq.Empty = Seq.Empty
move (xs :|> Free n) = move xs :|> Free n
move (xs :|> N file c) = case fit xs of
    Nothing -> move xs :|> N file c
    Just new -> move new :|> Free file
  where
    fit :: Seq Block -> Maybe (Seq Block)
    fit Seq.Empty = Nothing
    fit (Free n :<| xs)
      | n >= file = Just (N file c :<| Free (n - file) :<| xs)
      | otherwise = (Free n :<|) <$> fit xs
    fit (x :<| xs) = (x :<|) <$> fit xs


compact :: Int -> [Int] -> [Block]
compact _ [] = []
compact n (file:free:xs) = N file n : Free free : compact (n + 1) xs
compact n [x] = [N x n]

checksum :: [Block] -> [Maybe Int]
checksum [] = []
checksum (Free n : xs) = replicate n Nothing <> checksum xs
checksum (N m file : xs) = replicate m (Just file) <> checksum xs

smaller :: [Maybe Int] -> [Int]
smaller [] = []
smaller (Just n : xs) = n : smaller xs
smaller (Nothing : xs) = case dropWhileEnd isNothing xs of
    [] -> []
    xs -> (fromJust (last xs)) : smaller (dropEnd 1 xs)

