-- Programa que dibuixa una fila de 3 trafficLights

-- lights :: Int -> Drawing
-- lights 0 = blank
-- lights n = trafficLight <> translated 3 0 (lights (n-1))
-- myDrawing = lights 3
import Drawing

lightBulb :: Color -> Double -> Drawing
lightBulb c p = colored c (translated 0 (p) (solidCircle 1))

frameIn :: Drawing
frameIn = colored "gray" (solidRectangle 2.5 8)
frameOut :: Drawing
frameOut = colored "black" (rectangle 2.7 8.2)

frame = frameOut <> frameIn

trafficLight= lightBulb "red" 2.75 <> lightBulb "yellow" 0 <> lightBulb "green" (-2.75) <> frame

lightRow1 :: Double -> Drawing
lightRow1 0.0 = blank
lightRow1 n = translated (3*n) (-9.0) trafficLight <> lightRow1 (n-1.0)

lightRow2 :: Double -> Drawing
lightRow2 0.0 = blank
lightRow2	n = translated (3*n) 0.0 trafficLight <> lightRow2 (n-1.0)

lightRow3 :: Double -> Drawing
lightRow3 0.0 = blank
lightRow3	n = translated (3*n) 9.0 trafficLight <> lightRow3 (n-1.0)

main  :: IO()

main = svgOf (lightRow1 3.0 <> lightRow2 3.0 <> lightRow3 3.0)


--foldMap :: (a -> Drawing) -> [a] -> Drawing
