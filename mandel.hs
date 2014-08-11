

import Fractal
import Codec.Picture
import Data.Maybe
import Data.Complex
import Data.Default
import Control.Monad.State
import System.Directory

draw :: FractalOptions -> IO ()
draw opts = 
    let (w, h) = (200, 200)
        img = generateImage px w h
        px = fractal opts
                        { fractWidth = w
                        , fractHeight = h 
                        }

    in do
        savePngImage  "out_tmp.png" (ImageRGB8 img)
        copyFile "out_tmp.png" "out.png"



type FractalApp = StateT FractalOptions IO ()

main :: IO ()
main = runStateT fractalApp mandelbrot >> return ()

fractalApp :: FractalApp
fractalApp = forever $ do
    opts <- get
    liftIO $ draw opts
    liftIO $ putStrLn "Ready."
    commands <- liftIO getLine
    mapM_ handleAction commands
    return ()

handleAction :: Char -> FractalApp
handleAction k = do
    opts <- get
    let vDist = fromIntegral (fractHeight opts) / 10
    let hDist = fromIntegral (fractWidth opts) / 10
    when (k == 'h') $ liftIO . putStrLn $ "Zoom: - +   Move: W A S D   Detail: < >"
    put $ case k of 
            '=' -> zoom 2.0 opts
            '-' -> zoom 0.5 opts
            's' -> move (0 :+ vDist) opts
            'a' -> move ((-hDist) :+ 0) opts
            'w' -> move (0 :+ (-vDist)) opts
            'd' -> move (hDist :+ 0) opts
            ',' -> opts { fractMaxIter = round ( fromIntegral (fractMaxIter opts) / 2 ) }
            '.' -> opts { fractMaxIter = fractMaxIter opts * 2 }
            _   -> opts

zoom :: Double -> FractalOptions -> FractalOptions
zoom amount opts = opts { fractZoom = fractZoom opts * amount }

move :: Complex Double -> FractalOptions -> FractalOptions
move (x:+y) opts = opts { fractCenter = fractCenter opts + (x / fractZoom opts :+ y / fractZoom opts) }
