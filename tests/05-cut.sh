#!/bin/sh -ex
. ${0%05-cut.sh}setup.sh

nio z chF2.Ni4c Date.Ni > f3.Ni4Ci
nio c -d 0 f3.Ni4Ci | nio p .N4Ci
nio c -d 1 f3.Ni4Ci | nio p .Nii
nio c -d 2 f3.Ni4Ci | nio p .Ni4C
nio c -d 0:3 f3.Ni4Ci | nio p .Ni # no rows so fmt doesn't matter
nio c -d 0:2 f3.Ni4Ci | nio p .Ni
nio c -d 1:3 f3.Ni4Ci | nio p .Ni
nio c -d :1  f3.Ni4Ci | nio p .N4Ci
nio c -d :-1 f3.Ni4Ci | nio p .Ni
nio c -d 1:-1 f3.Ni4Ci | nio p .Nii
nio c -d 2:-1 f3.Ni4Ci | nio p .Ni4Ci
nio c -d 1:   f3.Ni4Ci | nio p .Ni
nio c -d 0:   f3.Ni4Ci | nio p .Ni
