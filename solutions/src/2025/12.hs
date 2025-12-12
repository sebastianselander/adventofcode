module Main where

import Advent.Format (format)

main :: IO ()
main = do
    (_, rows) <- [format|2025 12 ($(~(%u:)%n(%y+%n)*%n))*((%ux%u:)( %u)*%n)*|]
    print $ sum $ fmap (\((a, b), xs) -> fromEnum $ a * b >= sum xs * 9) rows
