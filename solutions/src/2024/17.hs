{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Main where

import Advent.Format (format)
import Control.Monad.State (MonadState, State, execState, get, gets, modify)
import Data.Array (Array, array, bounds, inRange, (!))
import Data.Bits (xor)
import Data.Function (fix)

main :: IO ()
main = do
    (a, _, _, instrs') <- [format|2024 17 Register A: %u%nRegister B: %u%nRegister C: %u%n%nProgram: %i&,%n|]
    let instructions = array (0 :: Int, length instrs' - 1) (zip [0 ..] instrs')
    let start = filter ((==0) . getb) [1..7]
    print $ runInterpreter (initProgram instructions a)
    print $ minimum (part2 instructions (snd (bounds instructions)) start)

data Program = Program
    { _instructions :: Array Int Int
    , _reg_a :: Int
    , _reg_b :: Int
    , _reg_c :: Int
    , _pointer :: Int
    , _out :: [Int]
    }
    deriving (Show)

initProgram :: Array Int Int -> Int -> Program
initProgram instructions a = Program instructions a 0 0 0 []

op_instruction :: State Program (Int, Int)
op_instruction = do
    prg <- get
    pure (prg._instructions ! prg._pointer, prg._instructions ! (prg._pointer + 1))

incPtr :: State Program ()
incPtr = do
    n <- gets _pointer
    setPtr (n + 2)

setPtr :: (MonadState Program m) => Int -> m ()
setPtr n = modify $ \s -> s{_pointer = n}

reg_a, reg_b, reg_c :: State Program Int
reg_a = gets _reg_a
reg_b = gets _reg_b
reg_c = gets _reg_c

put_a, put_b, put_c :: Int -> State Program ()
put_a x = modify $ \s -> s{_reg_a = x}
put_b x = modify $ \s -> s{_reg_b = x}
put_c x = modify $ \s -> s{_reg_c = x}

out :: Int -> State Program ()
out n = modify $ \s -> s{_out = n : s._out}

runInterpreter :: Program -> [Int]
runInterpreter program = reverse $ _out $ flip execState program $ do
    fix $ \loop -> do
        ptr <- gets _pointer
        is <- gets _instructions
        if not (inRange (bounds is) ptr)
            then pure ()
            else do
                interpret
                loop

interpret :: State Program ()
interpret = do
    (i, o) <- op_instruction
    case i of
        0 -> do
            a <- reg_a
            o' <- combo o
            put_a (a `div` (2 ^ o'))
            incPtr
        1 -> do
            b <- reg_b
            put_b (b `xor` o)
            incPtr
        2 -> do
            o' <- combo o
            put_b $ o' `mod` 8
            incPtr
        3 -> do
            a <- reg_a
            if a /= 0 then setPtr o else incPtr
        4 -> do
            b <- reg_b
            c <- reg_c
            put_b (xor b c)
            incPtr
        5 -> do
            o' <- combo o
            out $ o' `mod` 8
            incPtr
        6 -> do
            a <- reg_a
            o' <- combo o
            put_b (a `div` (2 ^ o'))
            incPtr
        7 -> do
            a <- reg_a
            o' <- combo o
            put_c (a `div` (2 ^ o'))
            incPtr
        _ -> error "bad input"

combo :: Int -> State Program Int
combo 0 = pure 0
combo 1 = pure 1
combo 2 = pure 2
combo 3 = pure 3
combo 4 = reg_a
combo 5 = reg_b
combo 6 = reg_c
combo _ = error "combo: bad input"

range :: Int -> [Int]
range n = [8 * n .. 8 * n + 7]

part2 :: Array Int Int -> Int -> [Int] -> [Int]
part2 _ 0 xs = xs
part2 arr n xs = part2 arr (n - 1) $ concatMap (filter ((== arr ! (n - 1)) . getb) . range) xs

getb :: Int -> Int
getb a =
    let b = (a `mod` 8) `xor` 3
        c = a `div` (2 ^ b)
        b' = b `xor` 5
     in (b' `xor` c) `mod` 8
