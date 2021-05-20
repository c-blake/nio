#!/bin/sh
# This is presently far from exhaustive.

(echo 1.23; echo 4.56; echo 7.89) | nio sc -if -of > kk.Nf
nio p kk.Nf

(echo 123; echo 456) | nio sc -id -oi > jj.Ni
nio p jj.Ni

ln -s jj.Ni jj.N2i
ln -s jj.Ni jj.Nii
nio p jj.N2i
nio p jj.Nii

(echo 1; echo 2; echo 3)|nio sc -if -og > ld.Ng
nio p ld.Ng

(echo 0; echo 3; echo 6)|nio sc -id -oi > foos.Ni
(echo 1; echo 2; echo 3)|nio sc -if -of > flt.Nf
printf abc > ch.Nc

nio zip ch.Nc ch.Nc ch.Nc > ch3.N3c
nio p ch3.N3c
(echo;echo;echo) | tr \\n \\0 > zero.Nc
nio zip ch.Nc ch.Nc ch.Nc zero.Nc > ch4.N4c
nio p ch4.N4c
ln -s ch4.N4c ch4.Ncccc
nio p ch4.Ncccc

(echo 1; echo 2; echo 3)|nio sc -id -oi > foo.Ni
nio zip ch4.N4c foo.Ni > chF.N4ci
nio zip foo.Ni ch4.N4c > chF2.Ni4c

ln -s foo.Ni foodot
echo i%b > .foodot
nio p foodot

(echo hi
 echo neighborino
 echo MoreThan256charsMoreThan256charsMoreThan256charsMoreThan256charsMoreThan256charsMoreThan256charsMoreThan256charsMoreThan256charsMoreThan256charsMoreThan256charsMoreThan256charsMoreThan256charsMoreThan256charsMoreThan256charsMoreThan256charsMoreThan256charsMoreThan256chars
) > strings
nio sc -ix -oi < strings -x@strs > strs.Ni
nio p strs.Ni@strs%s

nio sc -ix -oi < strings -x@strs.Dn > strsDn.Ni
nio p strsDn.Ni@strs.Dn%s

nio sc -ix -oi < strings -x@strs.D0 > strsD0.Ni
nio p strsD0.Ni@strs.D0%s

nio sc -ix -oi < strings -x@strs.LC > strsLC.Ni
nio p strsLC.Ni@strs.LC%s

nio sc -ix -oi < strings -x@strs.LS > strsLS.Ni
nio p strsLS.Ni@strs.LS%s

nio sc -ix -oi < strings -x@strs.N6C > strs6C.Ni
nio p strs6C.Ni@strs.N6C%s

nio d foo.Nifd9c
nio d -lc foo.Nifd9c

#XXX add tests for under/overflow checking

cat > zip.schema <<EOF
#Test schema
#--preproc=gzip -d \$1|c2tsv   # popen() preproc to make strict TSV
--nHeader=0                  # number of rows which are headers
--maxLog=100                 # limit same-kind log messages
--shared=zstrings.LS         # name of any common strings file
--zip                        # stdout zip mode not default rip mode
#name NC SC TRANSFORM:args   # NC=NIOcode; SC=(scan|src)Code like scan1
qty	i	d            # parse input as decimal; emit uint32
Px	f	f            # parse as float32; emit float32
_	ignore               # ignore an input column
Id	8C	c            # embedded char arrays pad-clipped but can
Date	i	x @datez.N9c #..be transformed via intern into *fixed*
City	i	x @citiez.Dn #..or *variable width* repositories,
Note	i	x @notez.LS  #..with maybe a shared common string repo.
EOF
out=$(nio f -oszip.schema)

( echo 1 2.99 abcde SPY 20210428 Pittsburgh active
  echo 2 3.99 fghij IBM 20210429 Chicago defunct
  echo 3 4.99 klmno AMD 20210430 Boston defunct ) | tr \  \\0 > data

nio f -szip.schema /dev/stdin < data > $out

cat > rip.schema <<EOF
#Test schema
#--preproc=gzip -d \$1|c2tsv   # popen() preproc to make strict TSV
--nHeader=0                  # number of rows which are headers
--maxLog=100                 # limit same-kind log messages
--shared=rstrings.LS         # name of any common strings file
#name NC SC TRANSFORM:args   # NC=NIOcode; SC=(scan|src)Code like scan1
qty	i	d            # parse input as decimal; emit uint32
Px	f	f            # parse as float32; emit float32
Ign	i	x @.         # ignore an input column
Id	8C	c            # embedded char arrays pad-clipped but can
Date	i	x @dates.N9c #..be transformed via intern into *fixed*
City	i	x @cities.Dn #..or *variable width* repositories,
Note	i	x @.         #..with maybe a shared common string repo.
EOF

nio f -srip.schema "" < data    # empty string another way to indicate stdin

mv Ign.Ni Ign
echo i@%s > .Ign
nio p -a rstrings.LS Ign
