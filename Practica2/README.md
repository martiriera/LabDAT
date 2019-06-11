# PRÀCTICA 2 - Introducció a Haskell
_Sergio Giménez i Martí Riera_

# Índex
* [PRÀCTICA 2 - Introducció a Haskell](#pràctica-2---introducció-a-haskell)
* [Índex](#índex)
* [Primera part](#primera-part)
   * [Exercici 2](#exercici-2)
   * [Exercici 3](#exercici-3)
   * [Exercici 4](#exercici-4)
   * [Exercici 5](#exercici-5)
   * [Exerecici 6](#exerecici-6)
   * [Conclusions](#conclusions)
* [Segona Part](#segona-part)



# Primera part

## Exercici 2

Realitzeu un programa que generi el dibuix d’un semàfor centrat a l’origen
amb 3 llums de colors vermell, groc i verd. Definiu una funció <code>lightBulb
</code>que dibuixi un llum, amb 2 paràmetres corresponents al color i la
posició en l’eix Y.


```haskell
import Drawing

lightBulb :: Color -> Double -> Drawing
lightBulb c p = colored c (translated 0 (p) (solidCircle 1))

marcIn :: Drawing
marcIn = colored "gray" (solidRectangle 2.5 8)
marcOut :: Drawing
marcOut = colored "black" (rectangle 2.7 8.2)

marc = marcOut <> marcIn

semafor= lightBulb "red" 2.75 <> lightBulb "yellow" 0 <> lightBulb "green"
(-2.75) <> marc

main  :: IO()

main = svgOf (semafor)
```

![Un_Semafor](/Practica2/img/un_semafor.png)


## Exercici 3

**Realitzeu un programa que dibuixi una array de 3 x 3 semàfors.**


```haskell
import Drawing

lightBulb :: Color -> Double -> Drawing
lightBulb c p = colored c (translated 0 (p) (solidCircle 1))

frameIn :: Drawing
frameIn = colored "gray" (solidRectangle 2.5 8)
frameOut :: Drawing
frameOut = colored "black" (rectangle 2.7 8.2)

frame = frameOut <> frameIn

trafficLight= lightBulb "red" 2.75 <> lightBulb "yellow" 0 <> lightBulb "green"
(-2.75) <> frame

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
```

![matriu_de_semafors](/Practica2/img/matriu_semafors.png)


## Exercici 4

1. **a. Realitzeu un programa que dibuixi l’arbre de la figura de 8 nivells.**

```haskell
import Drawing

dibuixaBranca :: Int -> Drawing
dibuixaBranca 0 = blank
dibuixaBranca n = polyline [(0,0),(0,1)] <> translated 0 1 (rotated (-pi/10) (dibuixaBranca (n-1))) <> translated 0 1 (rotated (pi/10) (dibuixaBranca (n-1)))

arbre = dibuixaBranca 8

main :: IO()
main = svgOf (arbre)
```


2. **b. Modifiqueu el programa de manera que dibuixi petites flors (simples
cercles grocs) en les branques.**

```haskell
import Drawing

dibuixaBranca :: Int -> Drawing
dibuixaBranca 0 = blank
dibuixaBranca 1 = colored "yellow" (solidCircle 0.25)
dibuixaBranca n = polyline [(0,0),(0,1)] <> translated 0 1 (rotated (-pi/10) (dibuixaBranca (n-1))) <> translated 0 1 (rotated (pi/10) (dibuixaBranca (n-1)))

arbre = dibuixaBranca 8

main :: IO()
main = svgOf (arbre)

--Molt semblant al exercici anterior, sols hem afegit un petit cercle al final
-- de cada branca (última iteració abans de la 0 o "blank")
```

![arbolitos](/Practica2/img/arbres.png)

## Exercici 5

**Completar el codi de la funció repeatDraw**

```haskell
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
light r c = translated ( 3 * fromIntegral c - 6) (8 * fromIntegral r - 16)
semafor

--lightBulb dibuixa una bombeta a partir d'un color i una posicio
lightBulb :: Color -> Double -> Drawing
lightBulb c p = colored c (translated 0 (p) (solidCircle 1))

marcIn :: Drawing
marcIn = colored "gray" (solidRectangle 2.5 8)
marcOut :: Drawing
marcOut = colored "black" (rectangle 2.7 8.2)

marc = marcOut <> marcIn

semafor= lightBulb "red" 2.75 <> lightBulb "yellow" 0 <> lightBulb "green"
(-2.75) <> marc

main :: IO()
main = svgOf (myDrawing)
```

## Exerecici 6

**Donada una llista de n punts dibuixar n semàfors situats en les corresponents posicions, usant les funcions <code>foldMap </code>i <code>trafficLight</code>.**


```haskell
import Drawing

marcIn = colored "gray" (solidRectangle 2.5 8)
marcOut = colored "black" (rectangle 2.7 8.2)

lightBulb c p = colored c (translated 0 (p) (solidCircle 1))

semafor= lightBulb "red" 2.75 <> lightBulb "yellow" 0 <> lightBulb "green"
(-2.75) <> marcIn <> marcOut

trafficLight :: (Double, Double) -> Drawing
trafficLight (x, y) = translated x y semafor

trafficLights :: [(Double, Double)] -> Drawing
trafficLights list = foldMap trafficLight list

listPosicions = [(-4,0), (0,0), (4,0), (-4,9), (0,9), (4,9), (-4,-9), (0,-9),
 (4,-9)]

--La llista ens permet definir les posicions dels semàfors de manera molt més detallada i també còmode de treballar

main :: IO ()
main = svgOf (trafficLights listPosicions)
```
## Conclusions

Com a conclusió de la pràctica podem dir que hem vist com usant funcions cada
cop més genèriques que ofereix Haskell, podem optimitzar el codi fent-lo més
curt i també llegible.

# Segona Part
