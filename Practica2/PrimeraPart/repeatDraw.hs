import Drawing

repeatDraw :: (Int -> Drawing) -> Int -> Drawing
repeatDraw thing 0 = blank
repeatDraw thing n =  thing n <> repeatDraw thing (n-1) 

myDrawing = repeatDraw lightRow 3 

lightRow :: Int -> Drawing
lightRow r = repeatDraw (light r) 3

light :: Int -> Int -> Drawing
light r c = translated ( 3 * fromIntegral c - 6) (8 * fromIntegral r - 16) semafor

lightBulb :: Color -> Double -> Drawing
lightBulb c p = colored c (translated 0 (p) (solidCircle 1))

marcIn :: Drawing
marcIn = colored "gray" (solidRectangle 2.5 8)
marcOut :: Drawing
marcOut = colored "black" (rectangle 2.7 8.2) 

marc = marcOut <> marcIn

semafor= lightBulb "red" 2.75 <> lightBulb "yellow" 0 <> lightBulb "green" (-2.75) <> marc

main :: IO()
main = svgOf (myDrawing)
