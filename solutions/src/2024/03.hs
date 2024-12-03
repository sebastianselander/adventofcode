module Main where

import Advent.Format (fmt, format)
import Control.Monad.Identity (Identity)
import Data.Either (lefts, partitionEithers)
import Data.Maybe (catMaybes)
import Text.Parsec (ParsecT, satisfy)

main :: IO ()
main = do
    input <- [format|2024 3 (mul\(%i,%i\)|(do\(\))!|(don't\(\))!|@t)*|]
    print $ eval True True $ g input
    print $ eval True False $ g input

data Instr = Mul Int Int | Do | Dont
    deriving (Show)

t :: ParsecT String u Identity Char
t = satisfy $ const True

g :: [Either (Either (Either (Int, Int) String) String) Char] -> [Instr]
g (Left (Left (Left (n, m))) : xs) = Mul n m : g xs
g (Left (Left (Right _)) : xs) = Do : g xs
g (Left (Right _) : xs) = Dont : g xs
g (_ : xs) = g xs
g [] = []


eval :: Bool -> Bool -> [Instr] -> Int
eval True to ((Mul x y) : xs) = x * y + eval True to xs
eval b to (Do : xs) = eval True to xs
eval b to (Dont : xs) = eval to to xs
eval b to (x : xs) = eval b to xs
eval b to [] = 0
