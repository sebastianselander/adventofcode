module Main where

import Advent.Format (format)
import Data.Array (Array, array, bounds, (!))
import Data.Bits (xor)

main :: IO ()
main = do
    (rega, instrs') <-
        [format|2024 17 Register A: %u%nRegister B: (~%u)%nRegister C: (~%u)%n%nProgram: %i&,%n|]
    let instructions = array (0 :: Int, length instrs' - 1) (zip [0 ..] instrs')
    let fth5 (_, _, _, _, e) = e
    let fst5 (a, _, _, _, _) = a
    print $
        reverse $
            fth5 $
                head $
                    dropWhile ((< 15) . fst5) $
                        iterate (step instructions) (0, rega, 0, 0, [])
    print $
        minimum
            (part2 instructions (snd (bounds instructions)) (filter ((== 0) . getb) [1 .. 7]))

step :: Array Int Int -> (Int, Int, Int, Int, [Int]) -> (Int, Int, Int, Int, [Int])
step is = go
  where
    go (pointer, a, b, c, out) = case is ! pointer of
        0 -> (pointer + 2, a `div` 2 ^ combo o, b, c, out)
        1 -> (pointer + 2, a, xor b o, c, out)
        2 -> (pointer + 2, a, combo o `mod` 8, c, out)
        3
            | a /= 0 -> (o, a, b, c, out)
            | otherwise -> (pointer + 2, a, b, c, out)
        4 -> (pointer + 2, a, xor b c, c, out)
        5 -> (pointer + 2, a, b, c, combo o `mod` 8 : out)
        6 -> (pointer + 2, a, a `div` 2 ^ combo o, c, out)
        7 -> (pointer + 2, a, b, a `div` 2 ^ combo o, out)
        _ -> error "bad input"
      where
        o = is ! (pointer + 1)
        combo 4 = a
        combo 5 = b
        combo 6 = c
        combo n = n

range :: Int -> [Int]
range n = [8 * n .. 8 * n + 7]

part2 :: Array Int Int -> Int -> [Int] -> [Int]
part2 _ 0 xs = xs
part2 arr n xs = part2 arr (n - 1) $ concatMap (filter ((== arr ! (n - 1)) . getb) . range) xs

getb :: Int -> Int
getb a = ((a `mod` 8 `xor` 3 `xor` 5) `xor` (a `div` 2 ^ (a `mod` 8 `xor` 3))) `mod` 8
