{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Main where

import Advent.Format (format, format')
import Advent.Prelude (fromDigits, toDigits)
import Control.Monad (when)
import Control.Monad.State (MonadState, State, evalState, execState, get, gets, modify)
import Data.Array (Array, array, bounds, inRange, (!))
import Data.Bits (xor)
import Data.Tuple.Extra (fst3, snd3, thd3)
import Debug.Trace (trace, traceShow)

main :: IO ()
main = do
    (a, b, c, instrs') <- [format|2024 17 Register A: %u%nRegister B: %u%nRegister C: %u%n%nProgram: %i&,%n|]
    let instrs = array (0 :: Int, length instrs' - 1) (zip [0 ..] instrs')
    print $ run instrs (a, b, c) 0

base8 :: [Int] -> Int
base8 = fromDigits 8

initProgram :: Array Int Int -> Int -> Program
initProgram instructions a = Program instructions a 0 0 0 []

data Program = Program
    { _instructions :: Array Int Int
    , _reg_a :: Int
    , _reg_b :: Int
    , _reg_c :: Int
    , _pointer :: Int
    , _out :: [Int]
    }
    deriving (Show)

op_instruction :: State Program (Int, Int)
op_instruction = do
    prg <- get
    pure (prg._instructions ! prg._pointer, prg._instructions ! (prg._pointer + 1))

incPtr :: State Program ()
incPtr = modify $ \s -> s{pointer = s.pointer + 2}

reg_a, reg_b, reg_c :: State Program Int
reg_a = gets _reg_a
reg_b = gets _reg_b
reg_c = gets _reg_c

put_a, put_b, put_c :: Int -> State Program ()
put_a x = modify $ \s -> s {_reg_a = x} 
put_b x = modify $ \s -> s {_reg_b = x} 
put_c x = modify $ \s -> s {_reg_c = x} 

interpret :: State Program ()
interpret = do
    (i, o) <- op_instruction
    case i of
        0 -> undefined
    incPtr

combo :: Int -> State Program Int
combo 0 = 0
combo 1 = 1
combo 2 = 2
combo 3 = 3
combo 4 = reg_a 
combo 5 = reg_b
combo 6 = reg_c
combo _ = error "combo: bad input"

run :: Array Int Int -> (Int, Int, Int) -> Int -> [Int]
run instrs regs pointer = reverse $ flip execState [] $ go regs pointer
  where
    go :: (Int, Int, Int) -> Int -> State [Int] ()
    go regs pointer = do
        (pointer', regs') <- interpret instrs regs pointer
        when (inRange (bounds instrs) (fromIntegral pointer')) $ go regs' pointer'

-- interpret :: Array Int Int -> (Int, Int, Int) -> Int -> State [Int] (Int, (Int, Int, Int))
-- interpret instrs (a, b, c) pointer =
--     let (inst, operand) = (instrs ! pointer, instrs ! (pointer + 1))
--         pointer' = pointer + 2
--      in case inst of
--             0 -> pure (pointer', (a `div` 2 ^ combo (a, b, c) operand, b, c))
--             1 -> pure (pointer', (a, xor b operand, c))
--             2 -> pure (pointer', (a, combo (a, b, c) operand `mod` 8, c))
--             3
--                 | a == 0 -> pure (pointer', (a, b, c))
--                 | operand /= pointer -> pure (operand, (a, b, c))
--                 | otherwise -> pure (pointer', (a, b, c))
--             4 -> pure (pointer', (a, xor b c, c))
--             5 -> do
--                 emit (combo (a, b, c) operand `mod` 8)
--                 pure (pointer', (a, b, c))
--             6 -> pure (pointer', (a, a `div` 2 ^ combo (a, b, c) operand, c))
--             7 -> pure (pointer', (a, b, a `div` 2 ^ combo (a, b, c) operand))
--             v -> error $ "bad input: " <> show v

base2 :: Int -> [Int]
base2 = toDigits 2

base10 :: [Int] -> Int
base10 = fromDigits 2

emit :: (MonadState [a] m) => a -> m ()
emit e = modify $ \s -> e : s

combo :: (Int, Int, Int) -> Int -> Int
combo _ 0 = 0
combo _ 1 = 1
combo _ 2 = 2
combo _ 3 = 3
combo regs 4 = fst3 regs
combo regs 5 = snd3 regs
combo regs 6 = thd3 regs
combo _ v = error $ "combo: bad input: " <> show v
