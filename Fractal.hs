{-# LANGUAGE NoMonomorphismRestriction #-}

module Fractal
( 
    fractal
,   with
,   FractalOptions (..)
)
where


import Control.Applicative
import Codec.Picture
import GHC.Word
import Debug.Trace
import Data.Function
import Data.Complex
import Data.Maybe (fromMaybe)
import Data.List (find)
import Data.Default
import Control.Monad.Reader

data FractalOptions = FractalOptions 
    { fractWidth :: Int
    , fractHeight :: Int
    , fractZoom :: Float
    , fractCenterX :: Float
    , fractCenterY :: Float
    , fractMaxIter :: Int
    , fractBailout :: Float
    , fractFormula :: Complex Float -> Complex Float -> Complex Float
    , fractInsideCol :: PixelRGB8
    , fractOutsideCol :: Int -> PixelRGB8
} -- deriving (Show, Eq, Read)

instance Default FractalOptions where
    def = FractalOptions 
        { fractWidth = 200
        , fractHeight = 200
        , fractZoom = 100
        , fractCenterX = 0
        , fractCenterY = 0
        , fractMaxIter = 10
        , fractBailout = 4.0
        , fractFormula = \ c z -> z ** 2 + c
        , fractInsideCol = PixelRGB8 0 0 0
        , fractOutsideCol = \ n -> let r = fromIntegral $ (n * 3) `rem` 256
                                       g = fromIntegral $ (100 + n * 7) `rem` 256
                                       b = fromIntegral $ (230 + n * 13) `rem` 256
                                   in PixelRGB8 r g b
        }

with :: Default a => a
with = def

fractal :: FractalOptions -> Int -> Int -> PixelRGB8
fractal opts x y =
        let 
            (w, h)   = (fractWidth opts, fractHeight opts)
            (x0, y0) = (fractCenterX opts, fractCenterY opts)
            zoom     = fractZoom opts
            f        = fractFormula opts
            x'       = x0 + (fromIntegral x - fromIntegral w / 2) / zoom
            y'       = y0 + (fromIntegral y - fromIntegral h / 2) / zoom
            c        = x' :+ y'
            color Nothing  = fractInsideCol opts 
            color (Just numIters) = fractOutsideCol opts numIters
        in 
            color $ doIter opts (f c) c


doIter :: FractalOptions -> (Complex Float -> Complex Float) -> Complex Float -> Maybe Int
doIter opts f start = countIters . zip [0..] . take maxIter $ iterate f start
    where outsideMandel z = magnitude z > bailout
          countIters vs = fst <$> find (\v -> outsideMandel (snd v)) vs
          maxIter = fractMaxIter opts
          bailout = fractBailout opts





