#!/bin/sh -ex
. ${0%04-schemas.sh}setup.sh

#XXX add tests for under/overflow checking

cat > zip.sc <<EOF
#Test schema
#--preproc=gzip -d \$1|c2tsv   # popen() preproc to make strict TSV
--nHeader=0                  # number of rows which are headers
--maxLog=100                 # limit same-kind log messages
--shared=zstrings.LS         # name of any common strings file
--zip                        # stdout zip mode not default rip mode
#name	NC	SC TRANSFORM:args # NC=NIOcode; SC=(scan|src)Code like scan1
qty	i	d            # parse input as decimal; emit uint32
Px	f	f            # parse as float32; emit float32
_	ignore               # ignore an input column
Id	8C	c            # embedded char arrays pad-clipped but can
Date	i	x @datez.N9c #..be transformed via intern into *fixed*
City	i	x @citiez.Dn #..or *variable width* repositories,
Note	i	x @notez.LS  #..with maybe a shared common string repo.
EOF
out=$(nio f -oszip.sc)

( echo 1 2.99 abcde SPY 20210428 Pittsburgh active
  echo 2 3.99 fghij IBM 20210429 Chicago defunct
  echo 3 4.99 klmno AMD 20210430 Boston defunct ) | tr \  \\0 > data

nio f -s zip.sc /dev/stdin < data > $out

cat > rip.sc <<EOF
--nHeader=0              # number of rows which are headers
--shared=rstrings.LS     # name of any common strings file
qty    i    d            # parse input as decimal; emit uint32
Px     f    f            # parse as float32; emit float32
Ign    i    x @.         # ignore an input column
Id    8C    c            # embedded char arrays pad-clipped but can
Date   i    x @dates.N9c #..be transformed via intern into *fixed*
City   i    x @cities.Dn #..or *variable width* repositories,
Note   i    x @.         #..with maybe a shared common string repo.
EOF

nio f -s rip.sc "" < data       # empty string another way to indicate stdin

mv Ign.Ni Ign
echo i@%s > .Ign
nio p -a rstrings.LS Ign Note.Ni@%s

cat > sstr.sc <<EOF
--nHeader=0                  # number of rows which are headers
--zip                        # stdout zip mode not default rip mode
#name	NC	SC TRANSFORM:args # NC=NIOcode; SC=(scan|src)Code like scan1
qty	i	d            # parse input as decimal; emit uint32
Px	f	f            # parse as float32; emit float32
_	ignore               # ignore an input column
Id	i	x @sstr.N12c # Share one string repository among..
Date	i	x @sstr.N12c #..several columns.
City	i	x @sstr.N12c
Note	i	x @sstr.N12c
EOF

nio f -s sstr.sc < data > $(nio f -o -s sstr.sc)
