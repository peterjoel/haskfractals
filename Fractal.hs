{-# LANGUAGE NoMonomorphismRestriction #-}

module Fractal
( 
    fractal
,   FractalOptions (..)
,   mandelbrot
,   julia
)
where

import Codec.Picture
import GHC.Word
import Debug.Trace
import Data.Function
import Data.Complex
import Data.Maybe (fromMaybe)
import Data.List (find)
import Data.Default

data FractalOptions = FractalOptions 
    { fractWidth :: Int
    , fractHeight :: Int
    , fractZoom :: Double
    , fractCenter :: Complex Double
    , fractMaxIter :: Int
    , fractBailout :: Complex Double -> Bool
    , fractFormula :: Complex Double -> Complex Double -> Complex Double
    , fractConstant :: Complex Double -> Complex Double
    , fractInsideCol :: PixelRGB8
    , fractOutsideCol :: Int -> PixelRGB8
    }


defaultOptions :: FractalOptions 
defaultOptions = FractalOptions 
        { fractWidth = 200
        , fractHeight = 200
        , fractZoom = 100
        , fractCenter = 0 :+ 0
        , fractMaxIter = 40
        , fractBailout = \ z -> magnitude z > 4.0
        , fractFormula = const
        , fractConstant = id
        , fractInsideCol = PixelRGB8 0 0 0
        , fractOutsideCol = \ n -> let r = fromIntegral $ (n * 3) `rem` 256
                                       g = fromIntegral $ (100 + n * 7) `rem` 256
                                       b = fromIntegral $ (230 + n * 13) `rem` 256
                                   in PixelRGB8 r g b
        }

instance Default FractalOptions where
    def = mandelbrot

mandelbrot :: FractalOptions
mandelbrot = defaultOptions { fractFormula = \c z -> z ** 2 + c }


julia :: Complex Double -> FractalOptions
julia c = mandelbrot { fractConstant = const c }


fractal :: FractalOptions -> Int -> Int -> PixelRGB8
fractal opts x y =
        let 
            (w, h)   = (fractWidth opts, fractHeight opts)
            (x0 :+ y0) = (fractCenter opts)
            zoom     = fractZoom opts
            f        = fractFormula opts
            x'       = x0 + (fromIntegral x - fromIntegral w / 2) / zoom
            y'       = y0 + (fromIntegral y - fromIntegral h / 2) / zoom
            z        = (x' :+ y')
            c        = fractConstant opts z
            color Nothing  = fractInsideCol opts 
            color (Just numIters) = fractOutsideCol opts numIters
        in 
            color $ doIter opts (f c) z


doIter :: FractalOptions -> (Complex Double -> Complex Double) -> Complex Double -> Maybe Int
doIter opts f start = countIters . zip [0..] . take maxIter $ iterate f start
    where countIters = fmap fst . find (fractBailout opts . snd)
          maxIter = fractMaxIter opts
          bailout = fractBailout opts



