#!/bin/sh -ex
. ${0%06-tails.sh}setup.sh
t=09
seq 0 9 | nio l -id -oi > $t.Ni
ns="0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 6 17 18 19"

# first test mman/rand access impls
for extra in "" "-c" "-r" "-rc"; do
  for n in $ns; do
    nio t -h$n $t.Ni | nio p .Ni
    nio t -t$n $t.Ni | nio p .Ni
    for m in $ns; do
      nio t -h$m -t$n $t.Ni | nio p .Ni
    done
  done
done

# then streaming impls
for extra in "" "-c" "-r" "-rc"; do
  for n in $ns; do
    nio t -h$n $t.Ni | nio p .Ni
    nio t -t$n $t.Ni | nio p .Ni
    for m in $ns; do
      cat $t.Ni | nio t -h$m -t$n .Ni | nio p .Ni
    done
  done
done
