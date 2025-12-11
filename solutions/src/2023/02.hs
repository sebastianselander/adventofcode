{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Advent.Format

data C = Cred | Cgreen | Cblue
    deriving (Show, Enum)

data Game = Game
    { ident :: Int
    , games :: [[(Int, C)]]
    }
    deriving (Show)

intro

main :: IO ()
main = do
    inp <- fmap (uncurry Game) <$> [format|2023 2 (Game %u: (((%u @C)&(, ))&(; ))%n)*|]
    print $ sum $ map (\(Game n _) -> n) $ filter (not . any (any (\(n, c) -> n > 12 + fromEnum c)) . games) inp
    print $ sum $ map (\xs -> maximum [r * g * b | (r, Cred) <- concat xs.games, (g, Cblue) <- concat xs.games, (b, Cgreen) <- concat xs.games]) inp
