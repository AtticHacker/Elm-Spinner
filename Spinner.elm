module Spinner where
import Window

sizes  = Window.dimensions
delta  = fps 65
delay  = 1
delay2 = 1.5

outLnHex color tuple = outlined (solid color) (hexagon tuple)
hexagon (p, s, x, y) = ngon p $ (x+y) `div` s

display (x, y) (i,_) (m,_) (q,_) (w,_) = collage x y
    [ rotate (degrees m)         $ outLnHex blue (5, 10, x, y)
    , rotate (degrees i)         $ outLnHex blue (5, 10, x, y)
    , rotate (degrees q)         $ outLnHex red  (4, 15, x, y)
    , rotate (degrees w)         $ outLnHex red  (4, 15, x, y)
    , rotate (degrees i)         $ outLnHex blue (3, 37, x, y)
    , rotate (degrees $ i - 90)  $ outLnHex blue (3, 37, x, y)
    , rotate (degrees $ i - 180) $ outLnHex blue (3, 37, x, y)
    , rotate (degrees $ i - 270) $ outLnHex blue (3, 37, x, y)
    , outLnHex blue (36, 37, x, y)
    ]

clock  _ (b, i) = if
    | b > 360   -> (b + i - 360, i)
    | otherwise -> (b + i, i)

clockC  _ (b, i) = if
    | b < 360   -> (b - i + 360, i)
    | otherwise -> (b - i, i)

main = display
    <~ sizes
     ~ foldp clock  (0, delay ) delta
     ~ foldp clockC (0, delay ) delta
     ~ foldp clock  (0, delay2) delta
     ~ foldp clockC (0, delay2) delta
