#!/bin/sh
set -e
./scrape-submit.sh `cat victory/unsubmitted`
mv victory/unsubmitted victory/unsubmitted'~'
