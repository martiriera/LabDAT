#!/bin/sh

echo "Content-Type: text/plain"
echo
v=$(cat visites.txt)
v=$(expr $v + 1)
echo $v > visites.txt
cat visites.txt

