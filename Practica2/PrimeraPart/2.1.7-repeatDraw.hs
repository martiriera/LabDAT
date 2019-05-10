import Drawing

--repeatDraw repeteix thing una quantitat de n vegades
repeatDraw :: (Int -> Drawing) -> Int -> Drawing
repeatDraw thing 0 = blank
repeatDraw thing n =  thing n <> repeatDraw thing (n-1) --Codi completat

myDrawing = repeatDraw lightRow 3

--lightRow dibuixa una fila de 3 semafors
--Apliquem light (2 args) parcialment ja que sols passem r
lightRow :: Int -> Drawing
lightRow r = repeatDraw (light r) 3

--light dibuixa el semafor (uneix 3 lightBulbs)
light :: Int -> Int -> Drawing
light r c = translated (4 * fromIntegral c - 6) (9 * fromIntegral r - 16) trafficLight

--lightBulb dibuixa una bombeta a partir d'un color i una posicio
lightBulb :: Color -> Double -> Drawing
lightBulb c p = colored c (translated 0 (p) (solidCircle 1))

frameIn :: Drawing
frameIn = colored "gray" (solidRectangle 2.5 8)
frameOut :: Drawing
frameOut = colored "black" (rectangle 2.7 8.2)

frame = frameOut <> frameIn

trafficLight= lightBulb "red" 2.75 <> lightBulb "yellow" 0 <> lightBulb "green" (-2.75) <> frame

main :: IO()
main = svgOf (myDrawing)
