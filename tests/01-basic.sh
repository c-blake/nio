#!/bin/sh -ex
. ${0%1-basic.sh}setup.sh

(echo 1.23; echo 4.56; echo 7.89) | nio l -if -of > kk.Nf
nio p kk.Nf

(echo 123; echo 456) | nio l -id -oi > jj.Ni
nio p jj.Ni

ln -s jj.Ni jj.N2i
ln -s jj.Ni jj.Nii
nio p jj.N2i
nio p jj.Nii

(echo 1; echo 2; echo 3)|nio l -if -og > ld.Ng
nio p ld.Ng

(echo 0; echo 3; echo 6)|nio l -id -oi > foos.Ni
(echo 1; echo 2; echo 3)|nio l -if -of > flt.Nf
printf abc > ch.Nc

nio zip ch.Nc ch.Nc ch.Nc > ch3.N3c
nio p ch3.N3c
(echo;echo;echo) | tr \\n \\0 > zero.Nc
nio zip ch.Nc ch.Nc ch.Nc zero.Nc > ch4.N4c
nio p ch4.N4c
ln -s ch4.N4c ch4.Ncccc
nio p ch4.Ncccc

(echo 1; echo 2; echo 3)|nio l -id -oi > foo.Ni
nio zip ch4.N4c foo.Ni > chF.N4ci
nio zip foo.Ni ch4.N4c > chF2.Ni4c

ln -s foo.Ni foodot
echo i%b > .foodot
nio p foodot
