module Main where

import Advent.Format
import Data.List (isPrefixOf)
import Text.RE.TDFA
import Data.Char (isDigit, digitToInt)
import Debug.Trace (traceShow)

main :: IO ()
main = do
    input <- readFile "/home/sebastian/Documents/git/adventofcode/inputs/2024/03.txt"
    print $ sum $ fmap eval $ instr True input

data Instr = Mul Int Int | BadMul Int Int
    deriving Show
toInt s 
  | all isDigit s = decimal $ fmap digitToInt s
  | otherwise = 0 

eval (Mul x y) = x * y
eval _ = 0
 
mul b = if b then Mul else BadMul 

instr :: Bool -> String -> [Instr]
instr lcont [] = []
instr lcont ('d':'o':'(':')':rest) = instr True rest
instr lcont ('d':'o':'n':'\'':'t':'(':')':rest) = instr False rest
instr lcont ('m':'u':'l':'(':x:',':a:')':rest) = mul lcont (toInt [x]) (toInt [a]) : instr lcont rest
instr lcont ('m':'u':'l':'(':x:',':a:b:')':rest) = mul lcont (toInt [x]) (toInt [a,b]) : instr lcont rest
instr lcont ('m':'u':'l':'(':x:',':a:b:c:')':rest) = mul lcont (toInt [x]) (toInt [a,b,c]) : instr lcont rest
instr lcont ('m':'u':'l':'(':x:y:',':a:')':rest) = mul lcont (toInt [x,y]) (toInt [a]) : instr lcont rest
instr lcont ('m':'u':'l':'(':x:y:',':a:b:')':rest) = mul lcont (toInt [x,y]) (toInt [a,b]) : instr lcont rest
instr lcont ('m':'u':'l':'(':x:y:',':a:b:c:')':rest) = mul lcont (toInt [x,y]) (toInt [a,b,c]) : instr lcont rest
instr lcont ('m':'u':'l':'(':x:y:z:',':a:')':rest) = mul lcont (toInt [x,y,z]) (toInt [a]) : instr lcont rest
instr lcont ('m':'u':'l':'(':x:y:z:',':a:b:')':rest) = mul lcont (toInt [x,y,z]) (toInt [a,b]) : instr lcont rest
instr lcont ('m':'u':'l':'(':x:y:z:',':a:b:c:')':rest) = mul lcont (toInt [x,y,z]) (toInt [a,b,c]) : instr lcont rest
instr lcont (x:xs) = instr lcont xs
