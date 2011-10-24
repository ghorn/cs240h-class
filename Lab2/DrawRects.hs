-- DrawRects.hs

{-# OPTIONS_GHC -Wall #-}

module Lab2.DrawRects( drawRects
                     ) where

import Lab2.Rect
import Graphics.Gloss

drawRects :: [Rect] -> Int -> IO ()
drawRects rects imageSize = do
  let myPicture :: Picture
      myPicture = Pictures $ map (r2p (getMbrs rects) imageSize) rects
  
  displayInWindow "FFFFFFUUUUUUUUUUU" (imageSize,imageSize) (20,600) black myPicture


r2p :: Rect -> Int -> Rect -> Picture
r2p mbr' imageSize rect = color aquamarine $ lineLoop coords
  where
    Rect { rectMaxX = maxX 
         , rectMinX = minX
         , rectMaxY = maxY
         , rectMinY = minY
         } = mbr'
      
    maxCoord = fromIntegral $ max (maxX - minX) (maxY - minY)
    
    coords :: [(Float, Float)]
    coords = map scaleCoords [ (rectMinX rect - minX, rectMinY rect - minY)
                             , (rectMinX rect - minX, rectMaxY rect - minY)
                             , (rectMaxX rect - minX, rectMaxY rect - minY)
                             , (rectMaxX rect - minX, rectMinY rect - minY)
                             , (rectMinX rect - minX, rectMinY rect - minY)
                             ]
      where
        scaleCoords :: (Int, Int) -> (Float, Float)
        scaleCoords (xi, yi) = (scaleCoord xi, scaleCoord yi)
          where
            scaleCoord :: Int -> Float
            scaleCoord ui = 0.5*(fromIntegral imageSize)*(-1.0 + 2.0*(fromIntegral ui)/maxCoord)



