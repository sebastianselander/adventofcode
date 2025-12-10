module Main where

import Advent.Format (format)
import Advent.PQueue qualified as Q
import Data.Array (Array, array, (!))
import Data.Bits (Bits (..))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Scientific (Scientific)
import Data.String (fromString)
import Data.Word (Word16)
import Numeric.Optimization.MIP qualified as MIP
import Numeric.Optimization.MIP ((.==.))
import Numeric.Optimization.MIP.Solver qualified as MIP

data Config = Config
    { lights :: (Word16, Int)
    , presses :: [[Int]]
    , joltage :: Array Int Int
    }
    deriving (Show)

main :: IO ()
main = do
    xs <- [format|2025 10 (\[(\.|#)!*\] (\(%u&(,)\) )*{(%u&,)}%n)*|]
    let configs =
            map
                ( \(as, ys, zs) ->
                    Config
                        (foldl' (\acc (i, x) -> if x == "." then setBit acc i else acc) 0 (zip [0 ..] as), length as)
                        ys
                        (array (0, length zs - 1) [(i, x) | (i, x) <- zip [0 ..] zs])
                )
                xs
    print $ sum (map solve configs)
    print =<< sum <$> mapM lp configs
    

lp :: Config -> IO Scientific
lp (Config _ presses joltage) = do
    sol <- MIP.solve MIP.cbc MIP.def problem
    pure (sum (Map.elems (MIP.solVariables sol)))
  where
    problem = 
        MIP.def
            { MIP.objectiveFunction =
                MIP.def
                    { MIP.objDir = MIP.OptMin
                    , MIP.objExpr = sum (map MIP.varExpr variables)
                    }
            , MIP.constraints = constraints
            , MIP.varDomains = Map.fromList [(v, (MIP.IntegerVariable, MIP.defaultBounds)) | v <- variables]
            }
    constraints =
        [ (sum (map (MIP.varExpr . (variables !!)) vs)) .==. (MIP.constExpr (fromIntegral $ joltage ! k))
        | (k, vs) <- Map.toList m
        ]
    m =
        foldr (\(k, v) acc -> Map.insertWith (++) k [v] acc) Map.empty $
            concat [map ((,i) . fromIntegral) p | (i, p) <- zip [0 ..] presses]
    variables = take (length presses) [(fromString [a]) | a <- ['a' ..]]

solve :: Config -> Int
solve (Config (originalLight, len) presses _) = go mempty maxBound (Q.singleton 0 (2 ^ len - 1, 0))
  where
    go :: Map Word16 Int -> Int -> Q.PQueue (Word16, Int) -> Int
    go _ best Q.Empty = best
    go seen best (l@(lights, n) Q.:<| xs)
        | best < n = go seen best xs
        | lights == originalLight = go (Map.insert lights (min best n) seen) (min best n) xs
        | Just m <- Map.lookup lights seen, m < n = go seen best xs
        | otherwise = go (Map.insert lights n seen) best (foldr ((\(arr, n) acc -> if n > best then acc else Q.insert n (arr, n) acc) . press l) xs presses)

press :: (Word16, Int) -> [Int] -> (Word16, Int)
press (light, n) xs = (foldl' complementBit light xs, n + 1)
