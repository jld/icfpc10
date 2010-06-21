#!/bin/sh
set -e

LAST=`tail -1 remote/carlist`
curl 'http://nfa.imn.htwk-leipzig.de/recent_cars/?G0='$LAST \
    | sed -n 's/^.*(\([0-9]*\),.*,&quot;\([0-2]*\)&quot;).*/\1 \2/p' \
    | while read carno carstr
do
    echo $carno >&2
    echo $carno >> remote/carlist
    echo $carstr > remote/cars/$carno
done
echo -- >&2
echo $LAST
