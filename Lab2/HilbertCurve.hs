-- HilbertCurve.hs

{-# OPTIONS_GHC -Wall #-}

module Lab2.HilbertCurve( HilbertCoord(..)
                        , makeHilbertCurve
                        , drawHilbertCurve
                        , xy2d
                        , d2xy
                        ) where

import Graphics.Gloss
import Data.Bits

data Direction = Direction Int Int

data HilbertCoord = HilbertCoord { hcX :: Int
                                 , hcY :: Int
                                 , hcV :: Int
                                 }
instance Show HilbertCoord where
  show hc = show (hcX hc, hcY hc, hcV hc)

data LindermayerVal = LM_A | LM_B | LM_F | LM_L | LM_R 
instance Show LindermayerVal where
  show LM_A = "A"
  show LM_B = "B"
  show LM_F = "F"
  show LM_L = "L"
  show LM_R = "R"


keepMe :: LindermayerVal -> Bool
keepMe LM_A = False
keepMe LM_B = False
keepMe _ = True

makeHilbertCurve :: Int -> [HilbertCoord]
makeHilbertCurve level = convertLindermayer $ filter keepMe $ lindermayerSubs level [LM_A]
  where
    lindermayerSubs 0 acc = acc
    lindermayerSubs n acc = lindermayerSubs (n-1) $ foldl lSub [] acc
      where
        lSub :: [LindermayerVal] -> LindermayerVal -> [LindermayerVal]
        lSub acc' LM_A  = acc' ++ [LM_L, LM_B, LM_F, LM_R, LM_A, LM_F, LM_A, LM_R, LM_F, LM_B, LM_L]
        lSub acc' LM_B  = acc' ++ [LM_R, LM_A, LM_F, LM_L, LM_B, LM_F, LM_B, LM_L, LM_F, LM_A, LM_R]
        lSub acc' other = acc' ++ [other]

convertLindermayer :: [LindermayerVal] -> [HilbertCoord]
convertLindermayer lVals = fst $ foldl executeLindermayer ([hc0], (Direction 0 1)) lVals
  where
    hc0 = HilbertCoord {hcX = 0, hcY = 0, hcV = 0}
    
    move :: Direction -> HilbertCoord -> HilbertCoord
    move (Direction dX dY)  hc = HilbertCoord {hcX = hcX hc + dX, hcY = hcY hc + dY, hcV = hcV hc + 1}
    
    turnRight :: Direction -> Direction
    turnRight (Direction x y) = Direction (-y) x
    
    turnLeft :: Direction -> Direction
    turnLeft (Direction x y) = Direction y (-x)
        
    executeLindermayer :: ([HilbertCoord], Direction) -> LindermayerVal -> ([HilbertCoord], Direction)
    executeLindermayer (hcs, dir) LM_L = (hcs, turnLeft dir)
    executeLindermayer (hcs, dir) LM_R = (hcs, turnRight dir)
    executeLindermayer (hcs, dir) LM_F = (hcs ++ [move dir (last hcs)], dir)
    executeLindermayer _ _ = error "didn't properly filter out LM_A/LM_B"

drawHilbertCurve :: Int -> [HilbertCoord] -> IO ()
drawHilbertCurve imageSize hCoords = do
  let maxCoord = fromIntegral $ maximum $ (map hcX hCoords) ++ (map hcY hCoords)

      scaleCoord :: Int -> Float
      scaleCoord ui = 0.4*(fromIntegral imageSize)*(-1.0 + 2.0*(fromIntegral ui)/maxCoord)
  
      coords = map (\hc -> (scaleCoord (hcX hc), scaleCoord (hcY hc))) hCoords
      myPicture :: Picture
      myPicture = color aquamarine $ line coords

  displayInWindow "FFFFFFUUUUUUUUUUU" (imageSize,imageSize) (20,600) black myPicture
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
