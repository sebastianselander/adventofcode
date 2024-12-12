{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Advent.Coord where

import Control.Monad.ST (ST, runST, stToIO)
import Data.Array.Base (IArray)
import Data.Array.Base qualified as AB
import Data.Array.IArray (array)
import Data.Array.IO.Internals qualified as AB
import Data.Data (Data)
import Data.Foldable (toList)
import Data.Map (Map, findWithDefault, keys, fromList)
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Base (Int (I#), indexIntArray#, readIntArray#, writeIntArray#, (*#), (+#))
import GHC.Generics (Generic)
import GHC.Ix (Ix (..), indexError)
import GHC.ST (ST (ST))

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

perimeter :: Set Coord -> [(Coord, Coord)]
perimeter s | Set.null s = []
perimeter coords = [(c, c - cx) | c <- toList coords, cx <- cardinalOn (`Set.notMember` coords) c]

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

nw :: Coord
nw = C (-1) (-1)

ne :: Coord
ne = C (-1) 1

sw :: Coord
sw = C 1 (-1)

se :: Coord
se = C 1 1

scale :: Int -> Coord -> Coord
scale n = mapCoord (n *)

-- | Precondition: Non-empty
coordArray :: (IArray a e) => [[e]] -> a Coord e
coordArray [] = error "coordMatrix: empty list"
coordArray xs@(x : _) = array (C 0 0, C (length xs - 1) (length x - 1)) $ coordLines xs

{- | Given a list of lines pair up each character with
its position.
-}
coordLines :: [[a]] -> [(Coord, a)]
coordLines rows = [(C y x, z) | (y, row) <- zip [0 ..] rows, (x, z) <- zip [0 ..] row]

cardinal :: Coord -> [Coord]
cardinal = cardinalOn (const True)

cardinalOn :: (Coord -> Bool) -> Coord -> [Coord]
cardinalOn f !c = [coord | coord <- [above c, right c, below c, left c], f coord]

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
        (C miny minx, C maxy maxx) ->
            unlines [[findWithDefault '·' (C y x) pixels | x <- [minx .. maxx]] | y <- [miny .. maxy]]

draw :: [Coord] -> String
draw [] = ""
draw zs = drawPicture (fromList $ fmap (,'#') zs)


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
boundingBox :: (Foldable f) => f Coord -> (Coord, Coord)
boundingBox t =
    case toList t of
        [] -> error "boundingBox: empty list"
        C y x : cs -> go y x y x cs
  where
    go loy lox hiy hix [] = lo `seq` hi `seq` (lo, hi)
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

instance Ix Coord where
    unsafeIndex (C lorow locol, C hirow hicol) (C row col) =
        unsafeIndex (lorow, hirow) row * unsafeRangeSize (locol, hicol) + unsafeIndex (locol, hicol) col
    {-# INLINE unsafeIndex #-}

    index b i
        | inRange b i = unsafeIndex b i
        | otherwise = indexError b i "Coord"
    {-# INLINE index #-}

    inRange (C lorow locol, C hirow hicol) (C row col) =
        inRange (lorow, hirow) row && inRange (locol, hicol) col
    {-# INLINE inRange #-}

    range (C lorow locol, C hirow hicol) =
        [C row col | row <- [lorow .. hirow], col <- [locol .. hicol]]
    {-# INLINE range #-}

    unsafeRangeSize (C lorow locol, C hirow hicol) =
        (hirow - lorow + 1) * (hicol - locol + 1)
    {-# INLINE unsafeRangeSize #-}

instance AB.IArray AB.UArray Coord where
    {-# INLINE bounds #-}
    bounds (AB.UArray l u _ _) = (l, u)
    {-# INLINE numElements #-}
    numElements (AB.UArray _ _ n _) = n
    {-# INLINE unsafeArray #-}
    unsafeArray lu ies = runST (AB.unsafeArrayUArray lu ies 0)
    {-# INLINE unsafeAt #-}
    unsafeAt (AB.UArray _ _ _ arr#) (I# i#) =
        C
            (I# (indexIntArray# arr# (2# *# i#)))
            (I# (indexIntArray# arr# (2# *# i# +# 1#)))
    {-# INLINE unsafeReplace #-}
    unsafeReplace arr ies = runST (AB.unsafeReplaceUArray arr ies)
    {-# INLINE unsafeAccum #-}
    unsafeAccum f arr ies = runST (AB.unsafeAccumUArray f arr ies)
    {-# INLINE unsafeAccumArray #-}
    unsafeAccumArray f initialValue lu ies = runST (AB.unsafeAccumArrayUArray f initialValue lu ies)

instance AB.MArray (AB.STUArray s) Coord (ST s) where
    {-# INLINE getBounds #-}
    getBounds (AB.STUArray l u _ _) = return (l, u)
    {-# INLINE getNumElements #-}
    getNumElements (AB.STUArray _ _ n _) = return n
    {-# INLINE unsafeNewArray_ #-}
    unsafeNewArray_ (l, u) = AB.unsafeNewArraySTUArray_ (l, u) (\x -> 2# *# AB.wORD_SCALE x)
    {-# INLINE newArray_ #-}
    newArray_ arrBounds = AB.newArray arrBounds 0
    {-# INLINE unsafeRead #-}
    unsafeRead (AB.STUArray _ _ _ marr#) (I# i#) = ST $ \s1# ->
        case readIntArray# marr# (2# *# i#) s1# of
            (# s2#, y# #) ->
                case readIntArray# marr# (2# *# i# +# 1#) s2# of
                    (# s3#, x# #) ->
                        (# s3#, C (I# y#) (I# x#) #)
    {-# INLINE unsafeWrite #-}
    unsafeWrite (AB.STUArray _ _ _ marr#) (I# i#) (C (I# y#) (I# x#)) = ST $ \s1# ->
        case writeIntArray# marr# (2# *# i#) y# s1# of
            s2# ->
                case writeIntArray# marr# (2# *# i# +# 1#) x# s2# of
                    s3# ->
                        (# s3#, () #)

instance AB.MArray AB.IOUArray Coord IO where
    {-# INLINE getBounds #-}
    getBounds (AB.IOUArray arr) = stToIO (AB.getBounds arr)
    {-# INLINE getNumElements #-}
    getNumElements (AB.IOUArray arr) = stToIO (AB.getNumElements arr)
    {-# INLINE newArray #-}
    newArray lu initialValue = stToIO (AB.IOUArray <$> AB.newArray lu initialValue)
    {-# INLINE unsafeNewArray_ #-}
    unsafeNewArray_ lu = stToIO (AB.IOUArray <$> AB.unsafeNewArray_ lu)
    {-# INLINE newArray_ #-}
    newArray_ = AB.unsafeNewArray_
    {-# INLINE unsafeRead #-}
    unsafeRead (AB.IOUArray marr) i = stToIO (AB.unsafeRead marr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite (AB.IOUArray marr) i e = stToIO (AB.unsafeWrite marr i e)
