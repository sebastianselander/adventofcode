{-# LANGUAGE MultiWayIf #-}

module Main where

import Advent.Format (format, format')
import Advent.Prelude (fromDigits, toDigits)
import Control.Monad (when)
import Control.Monad.State (MonadState, State, execState, modify)
import Data.Array (Array, array, bounds, inRange, (!))
import Debug.Trace (trace, traceShow)

main :: IO ()
main = do
    (a, b, c, instrs') <- [format|2024 17 Register A: %u%nRegister B: %u%nRegister C: %u%n%nProgram: %i&,%n|]
    let instrs = array (0 :: Integer, fromIntegral $ length instrs' - 1) (zip [0 ..] (fmap fromIntegral instrs'))
    print $ run instrs (fromIntegral a, fromIntegral b, fromIntegral c) 0
    print $ bs False 100_000_000 0 (fmap fromIntegral instrs') instrs (fromIntegral a, fromIntegral b, fromIntegral c) 0

bs :: Bool -> Integer -> Integer -> [Integer] -> Array Integer Integer -> (Integer, Integer, Integer) -> Integer -> Integer
bs b acc n expect instrs regs pointer =
    let out = run instrs ((\(_, b, c) -> (n, b, c)) regs) pointer
        outN = base8 out
        expectedN = base8 expect
     in if
            -- | trace (show outN <> " " <> show expectedN) False -> undefined
            | expect == out -> n
            | b && outN < expectedN -> bs b acc (n + 1) expect instrs regs pointer
            | outN < expectedN -> bs b acc (n + acc) expect instrs regs pointer
            | outN > expectedN -> trace ("Higher: " <> show outN <> " " <> show expectedN) $ bs True 0 (n - 1_000_000) expect instrs regs pointer
            | otherwise -> error "wtf"

base8 :: [Integer] -> Integer
base8 = fromDigits 8

self :: Integer -> [Integer] -> Array Integer Integer -> (Integer, Integer, Integer) -> Integer -> Integer
self n expect instrs regs pointer =
    let out = run instrs ((\(_, b, c) -> (n, b, c)) regs) pointer
     in if out == expect then n else if n `mod` 10000 == 0 then trace ("Out:    " <> show out <> "\nExpect: " <> show expect) $ self (n + 1) expect instrs regs pointer else self (n + 1) expect instrs regs pointer

run :: Array Integer Integer -> (Integer, Integer, Integer) -> Integer -> [Integer]
run instrs regs pointer = reverse $ flip execState [] $ go regs pointer
  where
    go :: (Integer, Integer, Integer) -> Integer -> State [Integer] ()
    go regs pointer = do
        (pointer', regs') <- interpret instrs regs pointer
        when (inRange (bounds instrs) (fromIntegral pointer')) $ go regs' pointer'

interpret :: Array Integer Integer -> (Integer, Integer, Integer) -> Integer -> State [Integer] (Integer, (Integer, Integer, Integer))
interpret instrs (a, b, c) pointer =
    let (inst, operand) = (instrs ! pointer, instrs ! (pointer + 1))
        pointer' = pointer + 2
     in case inst of
            0 -> pure (pointer', (a `div` 2 ^ combo (a, b, c) operand, b, c))
            1 -> pure (pointer', (a, base10 $ xor (base2 b) (base2 operand), c))
            2 -> pure (pointer', (a, combo (a, b, c) operand `mod` 8, c))
            3
                | a == 0 -> pure (pointer', (a, b, c))
                | operand /= pointer -> pure (operand, (a, b, c))
                | otherwise -> pure (pointer', (a, b, c))
            4 -> pure (pointer', (a, base10 $ reverse $ take 3 $ reverse $ xor (base2 b) (base2 c), c))
            5 -> do
                emit (combo (a, b, c) operand `mod` 8)
                pure (pointer', (a, b, c))
            6 -> pure (pointer', (a, a `div` 2 ^ combo (a, b, c) operand, c))
            7 -> pure (pointer', (a, b, a `div` 2 ^ combo (a, b, c) operand))
            v -> error $ "bad input: " <> show v

base2 :: Integer -> [Integer]
base2 = toDigits 2

base10 :: [Integer] -> Integer
base10 = fromDigits 2

xor :: [Integer] -> [Integer] -> [Integer]
xor xs ys = uncurry go (pad xs ys)
  where
    pad xs ys = (replicate (max (length xs) (length ys) - length xs) 0 <> xs, replicate (max (length xs) (length ys) - length ys) 0 <> ys)
    go [] [] = []
    go (1 : xs) (0 : ys) = 1 : go xs ys
    go (0 : xs) (1 : ys) = 1 : go xs ys
    go (_ : xs) (_ : ys) = 0 : go xs ys
    go xs [] = undefined
    go [] ys = undefined

emit :: (MonadState [a] m) => a -> m ()
emit e = modify $ \s -> e : s

combo :: (Integer, Integer, Integer) -> Integer -> Integer
combo (a, b, c) 0 = 0
combo (a, b, c) 1 = 1
combo (a, b, c) 2 = 2
combo (a, b, c) 3 = 3
combo (a, b, c) 4 = a
combo (a, b, c) 5 = b
combo (a, b, c) 6 = c
combo _ v = error $ "combo: bad input: " <> show v
