--polyline[(0,0),(0,1)]
--polyline :: [Point] -> Drawing
--polyline :: Foldable l => l Point -> Drawing

import Drawing

dibuixaBranca :: Int -> Drawing
dibuixaBranca 0 = blank
dibuixaBranca n = polyline [(0,0),(0,1)] <> translated 0 1 (rotated (-pi/10) (dibuixaBranca (n-1))) <> translated 0 1 (rotated (pi/10) (dibuixaBranca (n-1)))

arbre = dibuixaBranca 8

main :: IO()
main = svgOf (arbre)


