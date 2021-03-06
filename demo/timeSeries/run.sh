#!/bin/sh
cd $(dirname $0)
sh clean.sh                             # Clear out any remnants

# Unpack data - timestamps on files matter for the DONE-state machine
tar xzpf qp-stocks.tgz

d=$(pwd) # Here we are $PWD might also work.
echo DIRECTORY CONTENTS BEFORE:
ls

# It is possible to pre-allocate and pad (generously even).
nio up -c'nio f -r$PWD -sqp.sc -d/tmp/up $F' -T"ix2tm.N15C" -I"ix2id.N8C" -i"sym.N8C" $d/qp-stocks0/@TM@

touch -r qp-stocks1/20210816-031111 DONE # signal to process only new

nio up -c'nio f -r$PWD -sqp.sc -d/tmp/up $F' -T"ix2tm.N15C" -I"ix2id.N8C" -i"sym.N8C" $d/qp-stocks1/@TM@

touch -r qp-stocks2/20210816-031151 DONE # signal to process only new

nio up -c'nio f -r$PWD -sqp.sc -d/tmp/up $F' -T"ix2tm.N15C" -I"ix2id.N8C" -i"sym.N8C" $d/qp-stocks2/@TM@

echo DIRECTORY CONTENTS AFTER:
ls
# Print out some files
echo
nio p -ff%.2f ix2tm.N* tmId/last.N*
echo
nio p -ff%.2f ix2id.N* idTm/last.N*

(cd idTm; ln -s ../ix2* .)  # nio.py currently needs this
(cd tmId; ln -s ../ix2* .)  # but can/should be smarter.
PYTHONPATH=../../utils DPATH=`pwd` python3 eg.py
