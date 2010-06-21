#!/bin/sh
set -x

curl --data-urlencode j_username="Celestial Dire Badger" --data-urlencode j_password="$PASSWORD" -c remote/pages/cjar http://icfpcontest.org/icfp10/static/j_spring_security_check

export JSESSIONID=`awk '/JSESSION/{print $7}' < remote/pages/cjar`
export LOWCAR=`./scrape-newlist.sh`
./bearmatic.native
./longcheesematic.native
./lazybearmatic.native
./twobearmatic.native
./turkeymatic.native
./cheesematic.native
./scrape-submit-all.sh

curl -b remote/pages/cjar http://icfpcontest.org/icfp10/static/j_spring_security_logout
