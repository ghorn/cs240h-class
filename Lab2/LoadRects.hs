-- LoadRects.hs

{-# OPTIONS_GHC -Wall #-}

module Lab2.LoadRects( loadRects
                     , stringToRect
                     ) where

import Lab2.Rect
import Data.Time.Clock
import Debug.Trace

-- parse the x1,y1,x2,y2,x3,y3,x4,y4 string
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
      | verbose && not validCoords && idx == -1 = trace interactiveWarningMsg rect
      | verbose && not validCoords = trace warningMsg rect
      | otherwise                  = rect
      where
        warningMsg = "Fixing invalid coordinates found on line "++show idx++
                     ", diff: "++show worstErr++
                     ", coords: "++show coords
        interactiveWarningMsg = "   (Bounding invalid rectangle)"


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
  
  startTime <- getCurrentTime
  input     <- readFile filename
  let rects = map (stringToRect verbose) (zip (lines input) [1..])
  endTime   <- length rects `seq` getCurrentTime
  let diffTime = (realToFrac $ diffUTCTime endTime startTime)::Double
  putStrLn $ "loaded "++show (length rects)++" rectangles in " ++ show diffTime ++ " seconds"

  return rects


