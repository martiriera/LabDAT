import Drawing

myDrawing :: Drawing
myDrawing = solidCircle 1

main  :: IO()
main = svgOf myDrawing


