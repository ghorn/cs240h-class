-- Rect.hs

{-# OPTIONS_GHC -Wall #-}

module Lab2.Rect( Rect(..)
                , HRect(..)
                , CanIntersect(..)
                , fromRect
                , getMbr
                , getMbrs
                , intersect
                ) where

import Lab2.Config(hilbertDim)
import Lab2.HilbertCurve(xy2d)

data HRect = HRect Int Rect deriving Show

data Rect = Rect { rectMinX :: Int
                 , rectMaxX :: Int
                 , rectMinY :: Int
                 , rectMaxY :: Int
                 }
instance Show Rect where
  show rect = "Rect("++
              show (rectMinX rect)++","++
              show (rectMaxX rect)++","++
              show (rectMinY rect)++","++
              show (rectMaxY rect)++")"

-- calculate hiblert value of rect and return both as hrect
fromRect :: Rect -> HRect
fromRect rect = HRect (xy2d hilbertDim (hx, hy)) rect
  where
    hx = rectMinX rect + ((rectMaxX rect - rectMinX rect) `div` 2)
    hy = rectMinY rect + ((rectMaxY rect - rectMinY rect) `div` 2)

-- find the minimum bounding box of two rectangles
getMbr :: Rect -> Rect -> Rect
getMbr a b = Rect { rectMinX = min (rectMinX a) (rectMinX b)
                  , rectMinY = min (rectMinY a) (rectMinY b)
                  , rectMaxX = max (rectMaxX a) (rectMaxX b)
                  , rectMaxY = max (rectMaxY a) (rectMaxY b)
                  }

-- find the minimum bounding box of a list of rectangles
getMbrs :: [Rect] -> Rect
getMbrs [] = error "can't getMbrs on empty list"
getMbrs [r] = r
getMbrs (x:xs) = getMbr x (getMbrs xs)


-- things which can intersect each other
class CanIntersect a where
  mbr :: a -> Rect
instance CanIntersect Rect where
  mbr x = x
instance CanIntersect HRect where
  mbr (HRect _ r) = r

-- return true if two rectangles intersect
intersect :: (CanIntersect a, CanIntersect b) => a -> b -> Bool
intersect a b = xIntersect && yIntersect
  where
    xIntersect = aMaxX > bMinX && bMaxX > aMinX
    yIntersect = aMaxY > bMinY && bMaxY > aMinY
    Rect { rectMinX = aMinX, rectMaxX = aMaxX, rectMinY = aMinY, rectMaxY = aMaxY } = mbr a
    Rect { rectMinX = bMinX, rectMaxX = bMaxX, rectMinY = bMinY, rectMaxY = bMaxY } = mbr b
