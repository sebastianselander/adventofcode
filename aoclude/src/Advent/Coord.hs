{-# LANGUAGE LambdaCase #-}

module Advent.Coord where

import Data.Data
import Data.Foldable (toList)
import Data.Map (Map, findWithDefault, keys)
import GHC.Generics

-- | (Row, Column)
data Coord = C !Int !Int
    deriving (Read, Ord, Eq, Generic, Data)

zipCoord :: (Int -> Int -> Int) -> Coord -> Coord -> Coord
zipCoord f (C row1 col1) (C row2 col2) = C (f row1 row2) (f col1 col2)

mapCoord :: (Int -> Int) -> Coord -> Coord
mapCoord f (C row col) = C (f row) (f col)

coordRow :: Coord -> Int
coordRow (C row _) = row

coordCol :: Coord -> Int
coordCol (C _ col) = col

above :: Coord -> Coord
above (C row col) = C (row - 1) col

below :: Coord -> Coord
below (C row col) = C (row + 1) col

left :: Coord -> Coord
left (C row col) = C row (col - 1)

right :: Coord -> Coord
right (C row col) = C row (col + 1)

invert :: Coord -> Coord
invert (C row col) = C col row

flipX :: Coord -> Coord
flipX (C row col) = C row (-col)

flipY :: Coord -> Coord
flipY (C row col) = C (-row) col

turnLeft :: Coord -> Coord
turnLeft (C row col) = C (-col) row

turnRight :: Coord -> Coord
turnRight (C row col) = C col (-row)

turnAround :: Coord -> Coord
turnAround (C row col) = C (-row) (-col)

norm1 :: Coord -> Int
norm1 (C row col) = abs row + abs col

manhattan :: Coord -> Coord -> Int
manhattan c1 c2 = norm1 (c1 - c2)

euclidean :: Coord -> Coord -> Double
euclidean c1 c2 = pyth (c1 - c2)
  where
    pyth (C l r) = sqrt (fromIntegral (l * l + r * r))

origin :: Coord
origin = C 0 0

north :: Coord
north = C (-1) 0

east :: Coord
east = C 0 1

south :: Coord
south = C 1 0

west :: Coord
west = C 0 (-1)

scale :: Int -> Coord -> Coord
scale n = mapCoord (n *)

{- | Given a list of lines pair up each character with
its position.
-}
coordLines :: [[a]] -> [(Coord, a)]
coordLines rows = [(C y x, z) | (y, row) <- zip [0 ..] rows, (x, z) <- zip [0 ..] row]

cardinal :: Coord -> [Coord]
cardinal = cardinalOn (const True)

cardinalOn :: (Coord -> Bool) -> Coord -> [Coord]
cardinalOn f !c = [coord | coord <- [above c, left c, below c, right c], f coord]

neighbors :: Coord -> [Coord]
neighbors = neighborsOn (const True)

contains :: (Coord, Coord) -> Coord -> Bool
contains (topLeft, bottomRight) coord =
    coordCol topLeft <= coordCol coord
        && coordRow topLeft <= coordRow coord
        && coordCol bottomRight >= coordCol coord
        && coordRow bottomRight >= coordRow coord

neighborsOn :: (Coord -> Bool) -> Coord -> [Coord]
neighborsOn f !c =
    [ coord
    | coord <-
        [ above c
        , left c
        , below c
        , right c
        , above (left c)
        , above (right c)
        , below (left c)
        , below (right c)
        ]
    , f coord
    ]

{- | Render a minimal bounding box containing all the characters
at the given coordinates. Empty space filled with space characters.
-}
drawPicture :: Map Coord Char -> String
drawPicture pixels =
    case boundingBox (keys pixels) of
        Nothing -> ""
        Just (C miny minx, C maxy maxx) ->
            unlines [[findWithDefault '·' (C y x) pixels | x <- [minx .. maxx]] | y <- [miny .. maxy]]

charToCoord :: Char -> Maybe Coord
charToCoord = \case
    '^' -> Just north
    '<' -> Just west
    'v' -> Just south
    '>' -> Just east
    _ -> Nothing

{- | Find the upper-left and lower-right coordinates that
inclusively contain all the coordinates in a list of
coordinates.
-}
boundingBox :: (Foldable f) => f Coord -> Maybe (Coord, Coord)
boundingBox t =
    case toList t of
        [] -> Nothing
        C y x : cs -> go y x y x cs
  where
    go loy lox hiy hix [] = lo `seq` hi `seq` Just (lo, hi)
      where
        lo = C loy lox
        hi = C hiy hix
    go loy lox hiy hix (C y x : cs) = go (min loy y) (min lox x) (max hiy y) (max hix x) cs

instance Num Coord where
    (+) = zipCoord (+)
    (-) = zipCoord (-)
    (*) = zipCoord (*)
    abs = mapCoord abs
    signum = mapCoord signum
    fromInteger = (\i -> C i i) . fromInteger

instance Show Coord where
    show (C row col) = "(" ++ show row ++ ", " ++ show col ++ ")"
