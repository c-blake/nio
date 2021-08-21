#!/bin/sh
N=30
if [ $# -gt 0 ]; then
    N=$1
    shift 1
fi
datGen $N 3     # generate 3 columns of data
datGen $N 1 3   # generate some group labels

# Massage a little into a multi-file/multi-column test case
nio rip -i abc.Nfff x.Nf y.Nf z.Nf
nio zip x.Nf y.Nf > xy.Nff
rm abc.Nfff x.Nf y.Nf

# All set up.  Now fire off some group stats; just 'a'/mean here.
nio kred -ga.Nl -s,= -sa z.Nf xy.Nff
