#!/bin/sh

# disable filename globbing
set -f

echo "Content-Type: text/html"
echo

echo "<HTML><HEAD>"
echo "<TITLE>CGI que dona la hora...</TITLE>"
echo "</HEAD>"
echo "<BODY>"
echo "<H1>Avui es "
date "+%A %d de %B del %Y"
echo "</H1>"
echo "<H1>I s&oacute;n les "
date "+%T"
echo "</H1>"
echo "</BODY>"
echo "</HTML>"
 



