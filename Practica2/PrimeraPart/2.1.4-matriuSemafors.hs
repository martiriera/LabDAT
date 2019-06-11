-- Programa que dibuixa una fila de 3 semafors

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

trafficLight = lightBulb "red" 2.75 <> lightBulb "yellow" 0 <> lightBulb "green" (-2.75) <> frame

rowTrafficLigtht1 :: Double -> Drawing
rowTrafficLigtht1 0.0 = blank
rowTrafficLigtht1 n = translated (3*n) (-9.0) trafficLight <> rowTrafficLigtht1 (n-1.0)

rowTrafficLigtht2 :: Double -> Drawing
rowTrafficLigtht2 0.0 = blank
rowTrafficLigtht2	n = translated (3*n) 0.0 trafficLight <> rowTrafficLigtht2 (n-1.0)

rowTrafficLigtht3 :: Double -> Drawing
rowTrafficLigtht3 0.0 = blank
rowTrafficLigtht3	n = translated (3*n) 9.0 trafficLight <> rowTrafficLigtht3 (n-1.0)

main  :: IO()

main = svgOf (rowTrafficLigtht1 3.0 <> rowTrafficLigtht2 3.0 <> rowTrafficLigtht3 3.0)


--foldMap :: (a -> Drawing) -> [a] -> Drawing
