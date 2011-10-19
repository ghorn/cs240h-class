-- WikipediaHilbertCurve.hs

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Lab2.WikipediaHilbertCurve( d2xy
                                 , xy2d
                                 , makeHilbertCurve
                                 ) where
import Lab2.HilbertCurve(HilbertCoord(..))
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import System.IO.Unsafe(unsafePerformIO)


foreign import ccall unsafe "wikipediaHilbertCurve.h d2xy" c_d2xy :: CInt -> CInt -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall unsafe "wikipediaHilbertCurve.h xy2d" c_xy2d :: CInt -> CInt -> CInt -> IO CInt

safeToCInt :: Int -> CInt
safeToCInt x
  | and [toInteger x <= maxCInt, toInteger x >= minCInt] = fromIntegral x
  | otherwise = error "Error - safeToCInt detected overflow"
  where
    maxCInt = toInteger $ (maxBound :: CInt)
    minCInt = toInteger $ (minBound :: CInt)


xy2d :: Int -> (Int, Int) -> Int
xy2d n (x,y) = unsafePerformIO $ do
  out <- c_xy2d (safeToCInt (2^n)) (safeToCInt x) (safeToCInt y)
  return (fromIntegral out)


d2xy :: Int -> Int -> (Int, Int)
d2xy n d = unsafePerformIO $ do
  xPtr <- new (0 :: CInt)
  yPtr <- new (0 :: CInt)
  
  c_d2xy (safeToCInt (2^n)) (safeToCInt d) xPtr yPtr
  
  xOut <- peek xPtr
  yOut <- peek yPtr
  
  return (fromIntegral xOut, fromIntegral yOut)

makeHilbertCurve :: Int -> [HilbertCoord]
makeHilbertCurve n = map (\((x,y), d) -> HilbertCoord {hcX = x, hcY = y, hcV = d}) $ zip xys ds
  where
    xys = map (d2xy n) ds
    ds = [0..2^(2*n)-1]
