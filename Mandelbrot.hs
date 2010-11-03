-- Mandelbrotset in Haskell.
--
-- TODO
--   * error message for command line parameters
{-
    ghc --make -threaded Mandelbrot
    ./ Mandelbrot 320 200 foo.pnm +RTS -N2 -RTS
-}

module Main where
import System.Environment
import Text.Printf
import Data.List
import Control.Parallel
import System.Time



-- A PPM file describes the filename and dimensions of an image. Since it is
-- monochrome, it stores the values of the pixels as booleans.
data PPM = PPMFile {
      ppmName   :: String
    , ppmWidth  :: Int
    , ppmHeight :: Int
    , ppmData   :: [Bool] -- Should be abstracted later to a generalized
                          -- image type.
} 

main :: IO ()
main = do
    -- Parse command line. No error messages!
    [width', height', filename] <- getArgs
    let width   = read width'  :: Double
        height  = read height' :: Double    
    
    t0 <- getClockTime 
    let temp = compute_Parallel_Mandelbrot width height
        ppm = pseq temp $ PPMFile filename (fromEnum width) (fromEnum height) temp
    writePPMFile ppm
    t1 <- getClockTime 
    
    putStrLn ("\n\n\ntime: " ++ show (secDiff t0 t1) ++ " seconds\n\n\n")

-- Writes a PPMFile.
writePPMFile :: PPM -> IO ()
writePPMFile (PPMFile name w h img) = do
   let header = printf "P1\n%d %d\n1\n" w h
       imgd   = intercalate " " $ map boolToNum img
   writeFile name (header ++ imgd)
 where boolToNum True  = "1"
       boolToNum False = "0"

-- computes the single pixels of the Mandelbrot
compute_Parallel_Mandelbrot :: Double -> Double -> [Bool]
compute_Parallel_Mandelbrot width height = par g ( pseq f (g ++ f))
    where points     = [(x,y)| y <- [0..height-1], x <- [0..width-1]]
          diffX      = 3.0 / (width-1)
          diffY      = 2.0 / (height-1)
          mpoints    = map (\(x,y) -> (-2 + x*diffX, -1 + y*diffY)) points
          mpoints_p1 = take (truncate $ width * height / 2) mpoints
          mpoints_p2 = drop (truncate $ width * height / 2) mpoints
          f         = function mpoints_p1
          g         = function mpoints_p2
                                    
function :: [(Double,Double)] -> [Bool]
function list = map (f 1000) list


-- Return true, if the point (x,y) is in the set. Tests for iter iterations.
f :: Int -> (Double, Double) -> Bool
f iter (cx,cy) = all (<4) $ take iter (map abs (iterate f' (0.0,0.0)))
  where f' (x,y)  = (x*x-y*y+cx, 2*x*y+cy)
        abs (x,y) = x*x+y*y


secDiff :: ClockTime -> ClockTime -> Float 
secDiff (TOD secs1 psecs1) (TOD secs2 psecs2) = fromInteger (psecs2 - psecs1) / 1e12 + fromInteger (secs2 - secs1)
