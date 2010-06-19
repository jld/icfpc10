#!/bin/sh
mkdir -p remote/pages
curl -b JSESSIONID=$JSESSIONID http://icfpcontest.org/icfp10/score/instanceTeamCount > remote/pages/car-list.html
perl -le 'undef $/; $_=<>; print $1 while m:instance/(\d+)/solve:g' < remote/pages/car-list.html > remote/carlist
