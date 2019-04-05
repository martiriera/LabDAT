import Drawing

lightBulb :: Color -> Double -> Drawing
lightBulb c p = colored c (translated 0 (p) (solidCircle 1))

marcIn :: Drawing
marcIn = colored "gray" (solidRectangle 2.5 8)
marcOut :: Drawing
marcOut = colored "black" (rectangle 2.7 8.2) 

marc = marcOut <> marcIn

semafor= lightBulb "red" 2.75 <> lightBulb "yellow" 0 <> lightBulb "green" (-2.75) <> marc

main  :: IO()

main = svgOf (semafor)


