#!/bin/sh
set -e
for car in "$@"
do
    curl -b JSESSIONID=$JSESSIONID --data-urlencode contents@victory/factories/$car http://icfpcontest.org/icfp10/instance/$car/solve > remote/pages/submit.html
    perl -le 'undef $/; $_=<>; m:class="errors">(.*?)</span>: ? print "ERROR: $1" : m:<div id="main"><div .*?>(.*?)</div>:s ? print $1 : exit 1' < remote/pages/submit.html
    echo
    echo
    sleep 5
done
