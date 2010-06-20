#!/bin/sh
set -e
{ xclip -o ; echo; } \
    | sed -n 's/^(\([0-9]*\),.*,"\([0-2]*\)")$/\1 \2/p' \
    | while read carno carstr
do
    echo $carno
    echo $carno >> remote/carlist
    echo $carstr > remote/cars/$carno
done
