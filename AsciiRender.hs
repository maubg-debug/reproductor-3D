module AsciiRender (Vec3,Color(..),Triangle(..),render,mag,norm) where

import Data.Array.MArray
import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad (when)
import Control.Monad.ST
import Data.List (sort)

fI = fromIntegral

-- Un vector 3D.
type Vec3 = (Float,Float,Float)

-- Un caracter para pintar un triángulo o, alternativamente, pintar usando el vector normal
data Color = Col Char | Norm deriving (Show)

-- Un triangulo se compone de 3 puntos (x, y, z) y un color
data Triangle = Triangle Vec3 Vec3 Vec3 Color deriving (Show)

-- Magnitud vectorial
mag :: Vec3 -> Float
mag (x,y,z) = sqrt (x*x + y*y + z*z)

norm :: Vec3 -> Vec3
norm (x,y,z) = let
    m = mag (x,y,z)
    in (x/m,y/m,z/m)

sortbyx :: Triangle -> Triangle
sortbyx (Triangle p1 p2 p3 c) = let
    [p1',p2',p3'] = sort [p1,p2,p3]
    in Triangle p1' p2' p3' c

cosineSim :: Vec3 -> Vec3 -> Float
cosineSim v@(x,y,z) u@(a,b,c) = (x*a + y*b + z*c) / (mag u * mag v)

normalToChar :: Vec3 -> Char
normalToChar v = let
    ops = ".,-~:;!*=#$@"
    p = round (fI (length ops - 1) * (cosineSim (-1,1,0) v *0.5 + 0.5))
    in ops !! p

normal :: Triangle -> Vec3
normal (Triangle (x1,y1,z1) (x2,y2,z2) (x3,y3,z3) col) = let
    (v1x,v1y,v1z) = (x2-x1 , y2-y1 , z2-z1)
    (v2x,v2y,v2z) = (x3-x1 , y3-y1 , z3-z1)
    in norm (v1y*v2z-v1z*v2y , v1z*v2x-v1x*v2z , v1x*v2y-v1y*v2x)

orientation :: Triangle -> Bool
orientation (Triangle (x1,y1,_) (x2,y2,_) (x3,y3,_) _) = (x2-x1)*(y3-y2) > (x3-x2)*(y2-y1)

pixelsInside :: Int -> Int -> Triangle -> [((Int,Int,Float),Char)]
pixelsInside sizex sizey tri = let
    (Triangle (x1,y1,z1) (x2,y2,z2) (x3,y3,z3) col) = sortbyx tri
    dx = 1.0 / fI sizex
    dy = 1.0 / fI sizey
    toIx x = max 0 $ min (sizex-1) $ floor (x / dx) :: Int
    toIy y = max 0 $ min (sizey-1) $ floor (y / dy) :: Int
    xxi = toIx (x1 + dx/2)
    xxf = toIx (x3 - dx/2)
    limy1 x
        | x <= x1   = y1
        | x >= x3   = y3
        | x <= x2   = y1 + (y2 - y1) * (x - x1) / (x2 - x1)
        | otherwise = y2 + (y3 - y2) * (x - x2) / (x3 - x2)
    limy2 x
        | x <= x1   = y1
        | x >= x3   = y3
        | otherwise = y1 + (y3 - y1) * (x - x1) / (x3 - x1)
    limsyy xx = let
        x = (fI xx + 0.5) * dx
        ya = limy1 x
        yb = limy2 x
        yi = min ya yb
        yf = max ya yb
        yyi = toIy (yi + dy/2)
        yyf = toIy (yf - dy/2)
        in (yyi,yyf)
    nor@(nx,ny,nz) = normal tri
    dp xx yy = let
        x = (fI xx + 0.5) * dx
        y = (fI yy + 0.5) * dy
        in z1 - (nx*(x-x1) + ny*(y-y1)) / nz
    ch = case col of
        Norm  -> normalToChar nor
        Col c -> c
    in [((xx,yy,dp xx yy), ch) | xx <- [xxi..xxf], let (yyi,yyf) = limsyy xx, yy <- [yyi..yyf]]

renderTriangles :: Int -> Int -> [Triangle] -> UArray (Int,Int) Char
renderTriangles sizex sizey tris = runSTUArray $ do
    zbuffer <- newArray ((0,0),(sizey-1,sizex-1)) (1.0/0.0) :: ST s (STUArray s (Int,Int) Float)
    cbuffer <- newArray ((0,0),(sizey-1,sizex-1)) ' '       :: ST s (STUArray s (Int,Int) Char)
    let pixs = concatMap (pixelsInside sizex sizey) tris
    mapM_ (\((x,y,z),c) -> do
        z0 <- readArray zbuffer (y,x)
        when (z<z0) $ do
            writeArray zbuffer (y,x) z
            writeArray cbuffer (y,x) c
        ) pixs
    return cbuffer

render :: Int -> Int -> [Triangle] -> String
render sizex sizey tris = let
    arr = renderTriangles sizex sizey (filter orientation tris)
    lines = take sizey $ map (take sizex) $ iterate (drop sizex) (elems arr)
    in unlines lines
