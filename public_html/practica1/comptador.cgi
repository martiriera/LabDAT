#!/bin/sh

v=$(cat visites.txt)
v=$(expr $v + 1)
echo $v > visites.txt

echo "Content-Type: text/html"
echo
echo "<HTML><HEAD>"
echo "<TITLE>CGI que dona el numero de visites...</TITLE>"
echo "</HEAD>"
echo "<BODY>"
echo "<H1>Numero de visites: "
echo "</H1>"
echo "$v"
echo "</BODY>"
echo "</HTML>"
