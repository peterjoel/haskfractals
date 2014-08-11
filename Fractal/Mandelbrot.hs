{-# LANGUAGE NoMonomorphismRestriction #-}

module Fractal.Mandelbrot (
    mandel
)
where

import Control.Applicative
import Codec.Picture
import GHC.Word
import Debug.Trace
import Data.Function
import Data.Complex
import Data.List ( find )
import Fractal

maxiter = 20 :: Int
bailout = 4

zoom = 100.0
center = (0, 0)

imgW = 900 :: Int
imgH = 900 :: Int


mandel_iter c z = z ** 2 + c

toComplexPlane :: Int -> Int -> Complex Float
toComplexPlane x y = x' :+ y'
              where x' = fst center + (fromIntegral x - fromIntegral imgW / 2) / zoom
                    y' = snd center + (fromIntegral y - fromIntegral imgH / 2) / zoom
              --where x' = realPart tl + realPart (br - tl) * (fromIntegral x / fromIntegral imgW)
              --      y' = imagPart tl + imagPart (br - tl) * (fromIntegral y / fromIntegral imgH)

doIter :: (Show a, RealFloat a) => (Complex a -> Complex a) -> Complex a -> Maybe Int
doIter f start = countIters . zip [0..] . take maxiter $ iterate f start
    where outsideMandel z = magnitude z > fromIntegral bailout
          countIters vs = fst <$> find (\v -> outsideMandel (snd v)) vs

mandel :: Int -> Int -> Maybe Int
mandel x y = doIter (mandel_iter c) c
        where c = toComplexPlane x y




