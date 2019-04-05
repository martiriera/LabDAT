-- Programa que dibuixa una fila de 3 semafors

-- lights :: Int -> Drawing
-- lights 0 = blank
-- lights n = trafficLight <> translated 3 0 (lights (n-1))
-- myDrawing = lights 3 
import Drawing

lightBulb :: Color -> Double -> Drawing
lightBulb c p = colored c (translated 0 (p) (solidCircle 1))

marcIn :: Drawing
marcIn = colored "gray" (solidRectangle 2.5 8)
marcOut :: Drawing
marcOut = colored "black" (rectangle 2.7 8.2) 

marc = marcOut <> marcIn

semafor= lightBulb "red" 2.75 <> lightBulb "yellow" 0 <> lightBulb "green" (-2.75) <> marc

matriuSemafors1 :: Double -> Drawing
matriuSemafors1 0.0 = blank
matriuSemafors1 n = translated (3*n) (-9.0) semafor <> matriuSemafors1 (n-1.0)

matriuSemafors2 :: Double -> Drawing
matriuSemafors2 0.0 = blank
matriuSemafors2	n = translated (3*n) 0.0 semafor <> matriuSemafors2 (n-1.0)

matriuSemafors3 :: Double -> Drawing
matriuSemafors3 0.0 = blank
matriuSemafors3	n = translated (3*n) 9.0 semafor <> matriuSemafors3 (n-1.0)

main  :: IO()

main = svgOf (matriuSemafors1 3.0 <> matriuSemafors2 3.0 <> matriuSemafors3 3.0)


--foldMap :: (a -> Drawing) -> [a] -> Drawing
