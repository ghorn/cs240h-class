-- Rect.hs

{-# OPTIONS_GHC -Wall #-}

module Lab2.Rect( Rect(..)
                , loadRects
                , drawRects
                , rectDims
                , getMbr
                ) where

import Graphics.Gloss
import Debug.Trace

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
r2p maxCoord imageSize rect = color aquamarine $ lineLoop coords
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



stringToRect :: Bool -> (String, Int) -> Rect
stringToRect verbose (str, idx) = safeRect
  where
    coords@[x1,y1,x2,y2,x3,y3,x4,y4] = read $ '[':str++"]"
    
    worstErr = min (sum (map abs [x1 - x2, x3 - x4, y2 - y3, y1 - y4]))
                   (sum (map abs [x2 - x3, x1 - x4, y1 - y2, y3 - y4]))
    
    validCoords = and [x1 == x2, x3 == x4, y2 == y3, y1 == y4] ||
                  and [x2 == x3, x1 == x4, y1 == y2, y3 == y4]

    rect = Rect { rectMinX = minimum [x1,x2,x3,x4]
                , rectMaxX = maximum [x1,x2,x3,x4]
                , rectMinY = minimum [y1,y2,y3,y4]
                , rectMaxY = maximum [y1,y2,y3,y4]
                }
                      
    safeRect
      | verbose && not validCoords = trace warningMsg rect
      | otherwise                  = rect
      where
        warningMsg = "Fixing invalid coordinates found on line "++show idx++
                     ", diff: "++show worstErr++
                     ", coords: "++show coords


-- Take command line arguments
-- Return list of rectangles.
-- Throw error if badly formatted command line arguments.
-- If verbose command line argument is found (-v), throw warnings for irregular rectangles
loadRects :: [String] -> IO [Rect]
loadRects args' = do
  let verbose = "-v" `elem` args'
      args = filter (/= "-v") args'
      filename
        | length args /= 1 = error "need exactly one command line argument for filename"
        | otherwise        = head args
  putStrLn $ "loading file \""++filename++"\""
  
  input <- readFile filename

  return $ map (stringToRect verbose) (zip (lines input) [1..])


getMbr :: Rect -> Rect -> Rect
getMbr a b = Rect { rectMinX = min (rectMinX a) (rectMinX b)
                  , rectMinY = min (rectMinY a) (rectMinY b)
                  , rectMaxX = max (rectMaxX a) (rectMaxX b)
                  , rectMaxY = max (rectMaxY a) (rectMaxY b)
                  }
