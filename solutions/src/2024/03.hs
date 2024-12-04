{-# LANGUAGE TemplateHaskell #-}

module Main where

import Advent.Format (format, intro)
import Data.Maybe (catMaybes)

data Enabled = Enabled_don't_LPAREN_RPAREN | Enabled_do_LPAREN_RPAREN 
    deriving (Show)

intro

main :: IO ()
main = do
    input <- [format|2024 3 (mul\(%i,%i\)|@Enabled|~%t)*|]
    print $ eval True True input
    print $ eval True False input

eval :: Bool -> Bool -> [Maybe (Either (Int, Int) Enabled)] -> Int
eval True to ((Just (Left (n, m))) : xs) = n * m + eval True to xs
eval b to ((Just (Right Enabled_do_LPAREN_RPAREN)) : xs) = eval True to xs
eval b to (Just (Right Enabled_don't_LPAREN_RPAREN) : xs) = eval to to xs
eval b to (_ : xs) = eval b to xs
eval b to [] = 0
