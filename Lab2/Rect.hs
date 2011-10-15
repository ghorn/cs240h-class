-- Rect.hs

{-# OPTIONS_GHC -Wall #-}

module Lab2.Rect( Rect(..)
                , loadRects
                , drawRects
                , rectDims
                ) where

import Graphics.Gloss
import Debug.Trace
import Data.Maybe

data Rect = Rect { rectMinX :: Int
                 , rectMaxX :: Int
                 , rectMinY :: Int
                 , rectMaxY :: Int
                 } deriving Show

rectDims :: Rect -> (Int, Int)
rectDims rect = (rectMaxX rect - rectMinX rect, rectMaxY rect - rectMinY rect)

drawRects :: [Rect] -> Int -> IO ()
drawRects rects imageSize = do
  let maxX = maximum $ map rectMaxX rects
      maxY = maximum $ map rectMaxX rects
      maxCoord = fromIntegral $ max maxX maxY
    
      myPicture :: Picture
      myPicture = Pictures $ map (r2p maxCoord imageSize) rects
  
  displayInWindow "FFFFFFUUUUUUUUUUU" (imageSize,imageSize) (20,600) black myPicture


r2p :: Float -> Int -> Rect -> Picture
r2p maxCoord imageSize rect = color aquamarine $ lineLoop $ coords
  where
    coords :: [(Float, Float)]
    coords = map scaleCoords [ (rectMinX rect, rectMinY rect)
                             , (rectMinX rect, rectMaxY rect)
                             , (rectMaxX rect, rectMaxY rect)
                             , (rectMaxX rect, rectMinY rect)
                             , (rectMinX rect, rectMinY rect)
                             ]
      where
        scaleCoords :: (Int, Int) -> (Float, Float)
        scaleCoords (xi, yi) = (scaleCoord xi, scaleCoord yi)
          where
            scaleCoord :: Int -> Float
            scaleCoord ui = 0.5*(fromIntegral imageSize)*(-1.0 + 2.0*(fromIntegral ui)/maxCoord)



stringToRect :: (String, Int) -> Maybe Rect
stringToRect (str, idx) = safeRect
  where
    coords@[x1,y1,x2,y2,x3,y3,x4,y4] = (read $ '[':str++"]")
    
    worstErr = min (sum (map abs [x1 - x2, x3 - x4, y2 - y3, y1 - y4]))
                   (sum (map abs [x2 - x3, x1 - x4, y1 - y2, y3 - y4]))
    
    validCoords = or [ and [x1 == x2, x3 == x4, y2 == y3, y1 == y4]
                     , and [x2 == x3, x1 == x4, y1 == y2, y3 == y4]
                     ]
    rect = Rect { rectMinX = minimum [x1,x2,x3,x4]
                , rectMaxX = maximum [x1,x2,x3,x4]
                , rectMinY = minimum [y1,y2,y3,y4]
                , rectMaxY = maximum [y1,y2,y3,y4]
                }
                      
    safeRect
      | validCoords = Just rect
      | otherwise   = trace errMsg Nothing
      where
        errMsg = "Dropping invalid coordinates found on line "++show idx++
                 ", diff: "++show worstErr++
                 ", coords: "++show coords


-- Take command line argument for filename to load.
-- Return list of rectangles.
-- Throw error if there is not exactly one command line argument.
-- Throw error if text file is not formatted as expected
loadRects :: [String] -> IO [Rect]
loadRects args = do
  let filename
        | length args /= 1 = error "need exactly one command line argument (name of a text file to load)"
        | otherwise        = head args
  putStrLn $ "loading file \""++filename++"\""
  
  input <- readFile filename

  return $ map fromJust $ filter isJust $ map stringToRect (zip (lines input) [1..])
