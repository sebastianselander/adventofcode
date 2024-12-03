module Main where

import Advent.Format (format, i, fmt)
import Control.Monad.Identity (Identity)
import Data.Maybe (catMaybes)
import Text.Parsec (
    between,
    char,
    choice,
    many,
    satisfy,
    string,
    try,
 )
import Text.Parsec.Prim (ParsecT)

t = many $ satisfy $ const True

main :: IO ()
main = do
    input <- catMaybes . [fmt|@instrs|] <$> [format|2024 3 @t|]
    print $ eval True True input
    print $ eval True False input

data Instr = Mul Int Int | Do | Dont
    deriving (Show)

eval :: Bool -> Bool -> [Instr] -> Int
eval True to ((Mul x y) : xs) = x * y + eval True to xs
eval b to (Do : xs) = eval True to xs
eval b to (Dont : xs) = eval to to xs
eval b to (x : xs) = eval b to xs
eval b to [] = 0

p :: ParsecT String () Identity (Maybe Instr)
p =
    choice
        [ try $ do
            string "mul"
            between (char '(') (char ')') $ do
                n <- i
                char ','
                Just . Mul n <$> i
        , Just Do <$ try (string "do()")
        , Just Dont <$ try (string "don't()")
        , Nothing <$ satisfy (const True)
        ]

instrs :: ParsecT String () Identity [Maybe Instr]
instrs = many p
