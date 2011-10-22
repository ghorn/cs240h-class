-- SlowHilbertCurve.hs

{-# OPTIONS_GHC -Wall #-}

module Lab2.SlowHilbertCurve( makeHilbertCurve
                            , drawHilbertCurve
                            ) where

import Lab2.HilbertCoord
import Graphics.Gloss
import Data.List(foldl')

data Direction = Direction Int Int

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
    lindermayerSubs 0 acc = tail acc
    lindermayerSubs n acc = lindermayerSubs (n-1) $ foldl' lSub [] acc
      where
        lSub :: [LindermayerVal] -> LindermayerVal -> [LindermayerVal]
        lSub acc' LM_A  = acc' ++ [LM_R, LM_B, LM_F, LM_L, LM_A, LM_F, LM_A, LM_L, LM_F, LM_B, LM_R]
        lSub acc' LM_B  = acc' ++ [LM_L, LM_A, LM_F, LM_R, LM_B, LM_F, LM_B, LM_R, LM_F, LM_A, LM_L]
        lSub acc' other = acc' ++ [other]

convertLindermayer :: [LindermayerVal] -> [HilbertCoord]
convertLindermayer lVals = fst $ foldl' executeLindermayer ([hc0], Direction 0 1) lVals
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

drawHilbertCurve :: Int -> Int -> IO ()
drawHilbertCurve imageSize n = do
  let hCoords = makeHilbertCurve n
  let maxCoord = fromIntegral $ maximum $ map hcX hCoords ++ map hcY hCoords

      scaleCoord :: Int -> Float
      scaleCoord ui = 0.4*(fromIntegral imageSize)*(-1.0 + 2.0*(fromIntegral ui)/maxCoord)
  
      coords = map (\hc -> (scaleCoord (hcX hc), scaleCoord (hcY hc))) hCoords
      myPicture :: Picture
      myPicture = color aquamarine $ line coords

  displayInWindow "FFFFFFUUUUUUUUUUU" (imageSize,imageSize) (20,600) black myPicture
