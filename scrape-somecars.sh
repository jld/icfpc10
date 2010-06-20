#!/bin/sh
mkdir -p remote/cars
cat remote/carlist | while read car
do
    [ -e remote/cars/$car ] && continue
    case $car in
       *[$1]) echo "Car $car" ;;
       *) echo "Skipping $car"; continue ;;
    esac
    curl -b JSESSIONID=$JSESSIONID http://icfpcontest.org/icfp10/instance/$car/solve/form > remote/pages/car-$car.html
    perl -le 'undef $/; $_=<>; m@Car:</label>([012]*)</div>@ && print $1' < remote/pages/car-$car.html | tee remote/cars/$car
    sleep 1
done
