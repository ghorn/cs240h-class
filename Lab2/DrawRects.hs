-- DrawRects.hs

{-# OPTIONS_GHC -Wall #-}

module Lab2.DrawRects( drawRects
                     ) where

import Lab2.Rect
import Graphics.Gloss

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



