import Drawing

cercleR :: Drawing
cercleR = colored "red" (translated 0 (2.75) (solidCircle 1))

cercleY :: Drawing
cercleY = colored "yellow" (solidCircle 1)

cercleG :: Drawing
cercleG = colored "green" (translated 0 (-2.75) (solidCircle 1))

marcIn :: Drawing
marcIn = colored "gray" (solidRectangle 2.5 8)
marcOut :: Drawing
marcOut = colored "black" (rectangle 2.7 8.2) 



marc = marcOut <> marcIn

semafor= cercleR <> cercleY <> cercleG <> marc

main  :: IO()

main = svgOf (semafor)
