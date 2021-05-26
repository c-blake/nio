#!/bin/sh -ex
here=$(cd $(dirname $0); pwd)
. ${0%trip.sh}setup.sh
cd $here
rm -rf "$d"                 # clean up
export d nio                # pass through
for tst in $here/[0-9]*-*.sh; do
    $tst
done
