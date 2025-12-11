module Main where

import Text.RE.TDFA.String (ed, (*=~/))
import Advent.Format (format)
import Control.Arrow ((&&&))
import Data.Char (digitToInt, isDigit)

main :: IO ()
main = do
    inp <- [format|2023 1 (%s%n)*|]
    let nums = sum . map ( uncurry (+) . ((10 *) . head &&& last) . map digitToInt . filter isDigit)
    print $ nums inp
    print $ nums (fmap replace inp)

replace :: String -> String
replace s =
    s
        *=~/ [ed|one///o1e|]
        *=~/ [ed|two///t2o|]
        *=~/ [ed|three///t3e|]
        *=~/ [ed|four///f4r|]
        *=~/ [ed|five///f5e|]
        *=~/ [ed|six///s6x|]
        *=~/ [ed|seven///s7n|]
        *=~/ [ed|eight///e8t|]
        *=~/ [ed|nine///n9e|]
