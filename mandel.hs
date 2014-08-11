

import Fractal
import Codec.Picture
import Data.Maybe

main = 
    let (w, h) = (250, 250)
        img = generateImage px w h
        px = fractal with { fractWidth = w
                          , fractHeight = h 
                          , fractCenterX = -0.5
                          , fractCenterY = -0.5
                          , fractZoom = 50
                          }

    in savePngImage  "mandel.png" (ImageRGB8 img)

