module Main where

import Advent.Format
import Data.Char (digitToInt, isDigit)
import Data.List (isPrefixOf)
import Debug.Trace (traceShow)
import Text.RE.TDFA

main :: IO ()
main = do
    input <- readFile "/home/sebastian/Documents/git/adventofcode/inputs/2024/03.txt"
    print $ sum (eval <$> instr True True input)
    print $ sum (eval <$> instr True False input)

data Instr = Mul Int Int | BadMul Int Int
    deriving (Show)
toInt s
    | all isDigit s = decimal $ fmap digitToInt s
    | otherwise = 0

eval :: Instr -> Int
eval (Mul x y) = x * y
eval _ = 0

mul :: Bool -> Int -> Int -> Instr
mul b = if b then Mul else BadMul

instr :: Bool -> Bool -> String -> [Instr]
instr lcont to [] = []
instr lcont to ('d' : 'o' : '(' : ')' : rest) = instr True to rest
instr lcont to ('d' : 'o' : 'n' : '\'' : 't' : '(' : ')' : rest) = instr to to rest
instr lcont to ('m' : 'u' : 'l' : '(' : x : ',' : a : ')' : rest) = mul lcont (toInt [x]) (toInt [a]) : instr lcont to rest
instr lcont to ('m' : 'u' : 'l' : '(' : x : ',' : a : b : ')' : rest) = mul lcont (toInt [x]) (toInt [a, b]) : instr lcont to rest
instr lcont to ('m' : 'u' : 'l' : '(' : x : ',' : a : b : c : ')' : rest) = mul lcont (toInt [x]) (toInt [a, b, c]) : instr lcont to rest
instr lcont to ('m' : 'u' : 'l' : '(' : x : y : ',' : a : ')' : rest) = mul lcont (toInt [x, y]) (toInt [a]) : instr lcont to rest
instr lcont to ('m' : 'u' : 'l' : '(' : x : y : ',' : a : b : ')' : rest) = mul lcont (toInt [x, y]) (toInt [a, b]) : instr lcont to rest
instr lcont to ('m' : 'u' : 'l' : '(' : x : y : ',' : a : b : c : ')' : rest) = mul lcont (toInt [x, y]) (toInt [a, b, c]) : instr lcont to rest
instr lcont to ('m' : 'u' : 'l' : '(' : x : y : z : ',' : a : ')' : rest) = mul lcont (toInt [x, y, z]) (toInt [a]) : instr lcont to rest
instr lcont to ('m' : 'u' : 'l' : '(' : x : y : z : ',' : a : b : ')' : rest) = mul lcont (toInt [x, y, z]) (toInt [a, b]) : instr lcont to rest
instr lcont to ('m' : 'u' : 'l' : '(' : x : y : z : ',' : a : b : c : ')' : rest) = mul lcont (toInt [x, y, z]) (toInt [a, b, c]) : instr lcont to rest
instr lcont to (x : xs) = instr lcont to xs
