-- HilbertCurve.hs

{-# OPTIONS_GHC -Wall #-}

module Lab2.HilbertCurve( makeHilbertCurve
                        , drawHilbertCurve
                        , xy2d
                        , d2xy
                        ) where

import Graphics.Gloss
import Data.Bits

import Lab2.HilbertCoord


drawHilbertCurve :: Int -> Int -> IO ()
drawHilbertCurve imageSize n = do
  let hCoords = makeHilbertCurve n
  let maxCoord = fromIntegral $ maximum $ (map hcX hCoords) ++ (map hcY hCoords)

      scaleCoord :: Int -> Float
      scaleCoord ui = 0.4*(fromIntegral imageSize)*(-1.0 + 2.0*(fromIntegral ui)/maxCoord)
  
      coords = map (\hc -> (scaleCoord (hcX hc), scaleCoord (hcY hc))) hCoords
      myPicture :: Picture
      myPicture = color aquamarine $ line coords

  displayInWindow "FFFFFFUUUUUUUUUUU" (imageSize,imageSize) (20,600) black myPicture


makeHilbertCurve :: Int -> [HilbertCoord]
makeHilbertCurve n = map f [0..2^(2*n)-1]
  where
    f d = HilbertCoord { hcX = fst (d2xy n d)
                       , hcY = snd (d2xy n d)
                       , hcV = d
                       }


xy2d :: Int -> (Int, Int) -> Int
xy2d n (x,y) = f (x,y) 0 ((2^n) `div` 2)
  where
    f :: (Int, Int) -> Int -> Int -> Int
    f _ d 0 = d
    f (x',y') d s = f (rot s (x',y') (rx, ry)) newD (s `div` 2)
      where
        newD = d + s*s*((3*rx) `xor` ry)
        rx
          | (x' .&. s) > 0 = 1
          | otherwise      = 0
        ry
          | (y' .&. s) > 0 = 1
          | otherwise      = 0


d2xy :: Int -> Int -> (Int, Int)
d2xy n d' = f (0,0) d' 1
  where
    f :: (Int, Int) -> Int -> Int -> (Int, Int)
    f (x',y') d s 
      | s >= (2^n) = (x', y')
      | otherwise  = f (x'' + s*rx, y'' + s*ry) (d `div` 4) (2*s)
      where
        rx = 1 .&. (d `div` 2)
        ry = 1 .&. (d `xor` rx)
        
        (x'', y'') = rot s (x',y') (rx, ry)


rot :: Int -> (Int, Int) -> (Int, Int) -> (Int, Int)
rot n (x, y) (rx, ry)      
  | (ry == 0) && (rx == 1) = (n-1-y, n-1-x)
  | (ry == 0)              = (y, x)
  | otherwise              = (x, y)
